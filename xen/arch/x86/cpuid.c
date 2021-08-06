#include <xen/init.h>
#include <xen/lib.h>
#include <xen/param.h>
#include <xen/sched.h>
#include <xen/nospec.h>
#include <asm/cpuid.h>
#include <asm/hvm/hvm.h>
#include <asm/hvm/nestedhvm.h>
#include <asm/hvm/svm/svm.h>
#include <asm/hvm/viridian.h>
#include <asm/hvm/vmx/vmcs.h>
#include <asm/paging.h>
#include <asm/processor.h>
#include <asm/xstate.h>

const uint32_t known_features[] = INIT_KNOWN_FEATURES;

static const uint32_t __initconst pv_max_featuremask[] = INIT_PV_MAX_FEATURES;
static const uint32_t hvm_shadow_max_featuremask[] = INIT_HVM_SHADOW_MAX_FEATURES;
static const uint32_t __initconst hvm_hap_max_featuremask[] =
    INIT_HVM_HAP_MAX_FEATURES;
static const uint32_t __initconst pv_def_featuremask[] = INIT_PV_DEF_FEATURES;
static const uint32_t __initconst hvm_shadow_def_featuremask[] =
    INIT_HVM_SHADOW_DEF_FEATURES;
static const uint32_t __initconst hvm_hap_def_featuremask[] =
    INIT_HVM_HAP_DEF_FEATURES;
static const uint32_t deep_features[] = INIT_DEEP_FEATURES;

static const struct feature_name {
    const char *name;
    unsigned int bit;
} feature_names[] __initconstrel = INIT_FEATURE_NAMES;

/*
 * Parse a list of cpuid feature names -> bool, calling the callback for any
 * matches found.
 *
 * always_inline, because this is init code only and we really don't want a
 * function pointer call in the middle of the loop.
 */
static int __init always_inline parse_cpuid(
    const char *s, void (*callback)(unsigned int feat, bool val))
{
    const char *ss;
    int val, rc = 0;

    do {
        const struct feature_name *lhs, *rhs, *mid = NULL /* GCC... */;
        const char *feat;

        ss = strchr(s, ',');
        if ( !ss )
            ss = strchr(s, '\0');

        /* Skip the 'no-' prefix for name comparisons. */
        feat = s;
        if ( strncmp(s, "no-", 3) == 0 )
            feat += 3;

        /* (Re)initalise lhs and rhs for binary search. */
        lhs = feature_names;
        rhs = feature_names + ARRAY_SIZE(feature_names);

        while ( lhs < rhs )
        {
            int res;

            mid = lhs + (rhs - lhs) / 2;
            res = cmdline_strcmp(feat, mid->name);

            if ( res < 0 )
            {
                rhs = mid;
                continue;
            }
            if ( res > 0 )
            {
                lhs = mid + 1;
                continue;
            }

            if ( (val = parse_boolean(mid->name, s, ss)) >= 0 )
            {
                callback(mid->bit, val);
                mid = NULL;
            }

            break;
        }

        /*
         * Mid being NULL means that the name and boolean were successfully
         * identified.  Everything else is an error.
         */
        if ( mid )
            rc = -EINVAL;

        s = ss + 1;
    } while ( *ss );

    return rc;
}

static void __init cf_check _parse_xen_cpuid(unsigned int feat, bool val)
{
    if ( !val )
        setup_clear_cpu_cap(feat);
    else if ( feat == X86_FEATURE_RDRAND &&
              (cpuid_ecx(1) & cpufeat_mask(X86_FEATURE_RDRAND)) )
        setup_force_cpu_cap(X86_FEATURE_RDRAND);
}

static int __init cf_check parse_xen_cpuid(const char *s)
{
    return parse_cpuid(s, _parse_xen_cpuid);
}
custom_param("cpuid", parse_xen_cpuid);

static bool __initdata dom0_cpuid_cmdline;
static uint32_t __initdata dom0_enable_feat[FSCAPINTS];
static uint32_t __initdata dom0_disable_feat[FSCAPINTS];

static void __init cf_check _parse_dom0_cpuid(unsigned int feat, bool val)
{
    __set_bit  (feat, val ? dom0_enable_feat  : dom0_disable_feat);
    __clear_bit(feat, val ? dom0_disable_feat : dom0_enable_feat );
}

static int __init cf_check parse_dom0_cpuid(const char *s)
{
    dom0_cpuid_cmdline = true;

    return parse_cpuid(s, _parse_dom0_cpuid);
}
custom_param("dom0-cpuid", parse_dom0_cpuid);

#define EMPTY_LEAF ((struct cpuid_leaf){})
static void zero_leaves(struct cpuid_leaf *l,
                        unsigned int first, unsigned int last)
{
    memset(&l[first], 0, sizeof(*l) * (last - first + 1));
}

struct cpuid_policy __read_mostly     raw_cpuid_policy,
                    __read_mostly    host_cpuid_policy;
#ifdef CONFIG_PV
struct cpuid_policy __read_mostly  pv_max_cpuid_policy;
struct cpuid_policy __read_mostly  pv_def_cpuid_policy;
#endif
#ifdef CONFIG_HVM
struct cpuid_policy __read_mostly hvm_max_cpuid_policy;
struct cpuid_policy __read_mostly hvm_def_cpuid_policy;
#endif

static void sanitise_featureset(uint32_t *fs)
{
    /* for_each_set_bit() uses unsigned longs.  Extend with zeroes. */
    uint32_t disabled_features[
        ROUNDUP(FSCAPINTS, sizeof(unsigned long)/sizeof(uint32_t))] = {};
    unsigned int i;

    for ( i = 0; i < FSCAPINTS; ++i )
    {
        /* Clamp to known mask. */
        fs[i] &= known_features[i];

        /*
         * Identify which features with deep dependencies have been
         * disabled.
         */
        disabled_features[i] = ~fs[i] & deep_features[i];
    }

    for_each_set_bit(i, (void *)disabled_features,
                     sizeof(disabled_features) * 8)
    {
        const uint32_t *dfs = x86_cpuid_lookup_deep_deps(i);
        unsigned int j;

        ASSERT(dfs); /* deep_features[] should guarentee this. */

        for ( j = 0; j < FSCAPINTS; ++j )
        {
            fs[j] &= ~dfs[j];
            disabled_features[j] &= ~dfs[j];
        }
    }
}

static void recalculate_xstate(struct cpuid_policy *p)
{
    uint64_t xstates = XSTATE_FP_SSE;
    uint32_t xstate_size = XSTATE_AREA_MIN_SIZE;
    unsigned int i, Da1 = p->xstate.Da1;

    /*
     * The Da1 leaf is the only piece of information preserved in the common
     * case.  Everything else is derived from other feature state.
     */
    memset(&p->xstate, 0, sizeof(p->xstate));

    if ( !p->basic.xsave )
        return;

    if ( p->basic.avx )
    {
        xstates |= X86_XCR0_YMM;
        xstate_size = max(xstate_size,
                          xstate_offsets[X86_XCR0_YMM_POS] +
                          xstate_sizes[X86_XCR0_YMM_POS]);
    }

    if ( p->feat.mpx )
    {
        xstates |= X86_XCR0_BNDREGS | X86_XCR0_BNDCSR;
        xstate_size = max(xstate_size,
                          xstate_offsets[X86_XCR0_BNDCSR_POS] +
                          xstate_sizes[X86_XCR0_BNDCSR_POS]);
    }

    if ( p->feat.avx512f )
    {
        xstates |= X86_XCR0_OPMASK | X86_XCR0_ZMM | X86_XCR0_HI_ZMM;
        xstate_size = max(xstate_size,
                          xstate_offsets[X86_XCR0_HI_ZMM_POS] +
                          xstate_sizes[X86_XCR0_HI_ZMM_POS]);
    }

    if ( p->feat.pku )
    {
        xstates |= X86_XCR0_PKRU;
        xstate_size = max(xstate_size,
                          xstate_offsets[X86_XCR0_PKRU_POS] +
                          xstate_sizes[X86_XCR0_PKRU_POS]);
    }

    p->xstate.max_size  =  xstate_size;
    p->xstate.xcr0_low  =  xstates & ~XSTATE_XSAVES_ONLY;
    p->xstate.xcr0_high = (xstates & ~XSTATE_XSAVES_ONLY) >> 32;

    p->xstate.Da1 = Da1;
    if ( p->xstate.xsaves )
    {
        p->xstate.xss_low   =  xstates & XSTATE_XSAVES_ONLY;
        p->xstate.xss_high  = (xstates & XSTATE_XSAVES_ONLY) >> 32;
    }
    else
        xstates &= ~XSTATE_XSAVES_ONLY;

    for ( i = 2; i < min(63ul, ARRAY_SIZE(p->xstate.comp)); ++i )
    {
        uint64_t curr_xstate = 1ul << i;

        if ( !(xstates & curr_xstate) )
            continue;

        p->xstate.comp[i].size   = xstate_sizes[i];
        p->xstate.comp[i].offset = xstate_offsets[i];
        p->xstate.comp[i].xss    = curr_xstate & XSTATE_XSAVES_ONLY;
        p->xstate.comp[i].align  = curr_xstate & xstate_align;
    }
}

/*
 * Misc adjustments to the policy.  Mostly clobbering reserved fields and
 * duplicating shared fields.  Intentionally hidden fields are annotated.
 */
static void recalculate_misc(struct cpuid_policy *p)
{
    p->basic.raw_fms &= 0x0fff0fff; /* Clobber Processor Type on Intel. */
    p->basic.apic_id = 0; /* Dynamic. */

    p->basic.raw[0x5] = EMPTY_LEAF; /* MONITOR not exposed to guests. */
    p->basic.raw[0x6] = EMPTY_LEAF; /* Therm/Power not exposed to guests. */

    p->basic.raw[0x8] = EMPTY_LEAF;

    /* TODO: Rework topology logic. */
    memset(p->topo.raw, 0, sizeof(p->topo.raw));

    p->basic.raw[0xc] = EMPTY_LEAF;

    p->extd.e1d &= ~CPUID_COMMON_1D_FEATURES;

    /* Most of Power/RAS hidden from guests. */
    p->extd.raw[0x7].a = p->extd.raw[0x7].b = p->extd.raw[0x7].c = 0;

    p->extd.raw[0x8].d = 0;

    switch ( p->x86_vendor )
    {
    case X86_VENDOR_INTEL:
        p->basic.l2_nr_queries = 1; /* Fixed to 1 query. */
        p->basic.raw[0x3] = EMPTY_LEAF; /* PSN - always hidden. */
        p->basic.raw[0x9] = EMPTY_LEAF; /* DCA - always hidden. */

        p->extd.vendor_ebx = 0;
        p->extd.vendor_ecx = 0;
        p->extd.vendor_edx = 0;

        p->extd.raw[0x1].a = p->extd.raw[0x1].b = 0;

        p->extd.raw[0x5] = EMPTY_LEAF;
        p->extd.raw[0x6].a = p->extd.raw[0x6].b = p->extd.raw[0x6].d = 0;

        p->extd.raw[0x8].a &= 0x0000ffff;
        p->extd.raw[0x8].c = 0;
        break;

    case X86_VENDOR_AMD:
    case X86_VENDOR_HYGON:
        zero_leaves(p->basic.raw, 0x2, 0x3);
        memset(p->cache.raw, 0, sizeof(p->cache.raw));
        zero_leaves(p->basic.raw, 0x9, 0xa);

        p->extd.vendor_ebx = p->basic.vendor_ebx;
        p->extd.vendor_ecx = p->basic.vendor_ecx;
        p->extd.vendor_edx = p->basic.vendor_edx;

        p->extd.raw_fms = p->basic.raw_fms;
        p->extd.raw[0x1].b &= 0xff00ffff;
        p->extd.e1d |= p->basic._1d & CPUID_COMMON_1D_FEATURES;

        p->extd.raw[0x8].a &= 0x0000ffff; /* GuestMaxPhysAddr hidden. */
        p->extd.raw[0x8].c &= 0x0003f0ff;

        p->extd.raw[0x9] = EMPTY_LEAF;

        zero_leaves(p->extd.raw, 0xb, 0x18);

        /* 0x19 - TLB details.  Pass through. */
        /* 0x1a - Perf hints.   Pass through. */

        p->extd.raw[0x1b] = EMPTY_LEAF; /* IBS - not supported. */
        p->extd.raw[0x1c] = EMPTY_LEAF; /* LWP - not supported. */
        p->extd.raw[0x1d] = EMPTY_LEAF; /* TopoExt Cache */
        p->extd.raw[0x1e] = EMPTY_LEAF; /* TopoExt APIC ID/Core/Node */
        p->extd.raw[0x1f] = EMPTY_LEAF; /* SEV */
        p->extd.raw[0x20] = EMPTY_LEAF; /* Platform QoS */
        break;
    }
}

static void __init calculate_raw_policy(void)
{
    struct cpuid_policy *p = &raw_cpuid_policy;

    x86_cpuid_policy_fill_native(p);

    /* Nothing good will come from Xen and libx86 disagreeing on vendor. */
    ASSERT(p->x86_vendor == boot_cpu_data.x86_vendor);
}

static void __init calculate_host_policy(void)
{
    struct cpuid_policy *p = &host_cpuid_policy;
    unsigned int max_extd_leaf;

    *p = raw_cpuid_policy;

    p->basic.max_leaf =
        min_t(uint32_t, p->basic.max_leaf,   ARRAY_SIZE(p->basic.raw) - 1);
    p->feat.max_subleaf =
        min_t(uint32_t, p->feat.max_subleaf, ARRAY_SIZE(p->feat.raw) - 1);

    max_extd_leaf = p->extd.max_leaf;

    /*
     * For AMD/Hygon hardware before Zen3, we unilaterally modify LFENCE to be
     * dispatch serialising for Spectre mitigations.  Extend max_extd_leaf
     * beyond what hardware supports, to include the feature leaf containing
     * this information.
     */
    if ( cpu_has_lfence_dispatch )
        max_extd_leaf = max(max_extd_leaf, 0x80000021);

    p->extd.max_leaf = 0x80000000 | min_t(uint32_t, max_extd_leaf & 0xffff,
                                          ARRAY_SIZE(p->extd.raw) - 1);

    cpuid_featureset_to_policy(boot_cpu_data.x86_capability, p);
    recalculate_xstate(p);
    recalculate_misc(p);

    /* When vPMU is disabled, drop it from the host policy. */
    if ( vpmu_mode == XENPMU_MODE_OFF )
        p->basic.raw[0xa] = EMPTY_LEAF;

    if ( p->extd.svm )
    {
        /* Clamp to implemented features which require hardware support. */
        p->extd.raw[0xa].d &= ((1u << SVM_FEATURE_NPT) |
                               (1u << SVM_FEATURE_LBRV) |
                               (1u << SVM_FEATURE_NRIPS) |
                               (1u << SVM_FEATURE_PAUSEFILTER) |
                               (1u << SVM_FEATURE_DECODEASSISTS));
        /* Enable features which are always emulated. */
        p->extd.raw[0xa].d |= ((1u << SVM_FEATURE_VMCBCLEAN) |
                               (1u << SVM_FEATURE_TSCRATEMSR));
    }
}

static void __init guest_common_default_feature_adjustments(uint32_t *fs)
{
    /*
     * IvyBridge client parts suffer from leakage of RDRAND data due to SRBDS
     * (XSA-320 / CVE-2020-0543), and won't be receiving microcode to
     * compensate.
     *
     * Mitigate by hiding RDRAND from guests by default, unless explicitly
     * overridden on the Xen command line (cpuid=rdrand).  Irrespective of the
     * default setting, guests can use RDRAND if explicitly enabled
     * (cpuid="host,rdrand=1") in the VM's config file, and VMs which were
     * previously using RDRAND can migrate in.
     */
    if ( boot_cpu_data.x86_vendor == X86_VENDOR_INTEL &&
         boot_cpu_data.x86 == 6 && boot_cpu_data.x86_model == 0x3a &&
         cpu_has_rdrand && !is_forced_cpu_cap(X86_FEATURE_RDRAND) )
        __clear_bit(X86_FEATURE_RDRAND, fs);

    /*
     * On certain hardware, speculative or errata workarounds can result in
     * TSX being placed in "force-abort" mode, where it doesn't actually
     * function as expected, but is technically compatible with the ISA.
     *
     * Do not advertise RTM to guests by default if it won't actually work.
     */
    if ( rtm_disabled )
        __clear_bit(X86_FEATURE_RTM, fs);
}

static void __init guest_common_feature_adjustments(uint32_t *fs)
{
    /* Unconditionally claim to be able to set the hypervisor bit. */
    __set_bit(X86_FEATURE_HYPERVISOR, fs);

    /*
     * If IBRS is offered to the guest, unconditionally offer STIBP.  It is a
     * nop on non-HT hardware, and has this behaviour to make heterogeneous
     * setups easier to manage.
     */
    if ( test_bit(X86_FEATURE_IBRSB, fs) )
        __set_bit(X86_FEATURE_STIBP, fs);
    if ( test_bit(X86_FEATURE_IBRS, fs) )
        __set_bit(X86_FEATURE_AMD_STIBP, fs);

    /*
     * On hardware which supports IBRS/IBPB, we can offer IBPB independently
     * of IBRS by using the AMD feature bit.  An administrator may wish for
     * performance reasons to offer IBPB without IBRS.
     */
    if ( host_cpuid_policy.feat.ibrsb )
        __set_bit(X86_FEATURE_IBPB, fs);
}

static void __init calculate_pv_max_policy(void)
{
    struct cpuid_policy *p = &pv_max_cpuid_policy;
    uint32_t pv_featureset[FSCAPINTS];
    unsigned int i;

    *p = host_cpuid_policy;
    cpuid_policy_to_featureset(p, pv_featureset);

    for ( i = 0; i < ARRAY_SIZE(pv_featureset); ++i )
        pv_featureset[i] &= pv_max_featuremask[i];

    /*
     * If Xen isn't virtualising MSR_SPEC_CTRL for PV guests (functional
     * availability, or admin choice), hide the feature.
     */
    if ( !boot_cpu_has(X86_FEATURE_SC_MSR_PV) )
    {
        __clear_bit(X86_FEATURE_IBRSB, pv_featureset);
        __clear_bit(X86_FEATURE_IBRS, pv_featureset);
    }

    guest_common_feature_adjustments(pv_featureset);

    sanitise_featureset(pv_featureset);
    cpuid_featureset_to_policy(pv_featureset, p);
    recalculate_xstate(p);

    p->extd.raw[0xa] = EMPTY_LEAF; /* No SVM for PV guests. */
}

static void __init calculate_pv_def_policy(void)
{
    struct cpuid_policy *p = &pv_def_cpuid_policy;
    uint32_t pv_featureset[FSCAPINTS];
    unsigned int i;

    *p = pv_max_cpuid_policy;
    cpuid_policy_to_featureset(p, pv_featureset);

    for ( i = 0; i < ARRAY_SIZE(pv_featureset); ++i )
        pv_featureset[i] &= pv_def_featuremask[i];

    guest_common_feature_adjustments(pv_featureset);
    guest_common_default_feature_adjustments(pv_featureset);

    sanitise_featureset(pv_featureset);
    cpuid_featureset_to_policy(pv_featureset, p);
    recalculate_xstate(p);
}

static void __init calculate_hvm_max_policy(void)
{
    struct cpuid_policy *p = &hvm_max_cpuid_policy;
    uint32_t hvm_featureset[FSCAPINTS];
    unsigned int i;
    const uint32_t *hvm_featuremask;

    *p = host_cpuid_policy;
    cpuid_policy_to_featureset(p, hvm_featureset);

    hvm_featuremask = hvm_hap_supported() ?
        hvm_hap_max_featuremask : hvm_shadow_max_featuremask;

    for ( i = 0; i < ARRAY_SIZE(hvm_featureset); ++i )
        hvm_featureset[i] &= hvm_featuremask[i];

    /*
     * Xen can provide an (x2)APIC emulation to HVM guests even if the host's
     * (x2)APIC isn't enabled.
     */
    __set_bit(X86_FEATURE_APIC, hvm_featureset);
    __set_bit(X86_FEATURE_X2APIC, hvm_featureset);

    /*
     * We don't support EFER.LMSLE at all.  AMD has dropped the feature from
     * hardware and allocated a CPUID bit to indicate its absence.
     */
    __set_bit(X86_FEATURE_NO_LMSL, hvm_featureset);

    /*
     * On AMD, PV guests are entirely unable to use SYSENTER as Xen runs in
     * long mode (and init_amd() has cleared it out of host capabilities), but
     * HVM guests are able if running in protected mode.
     */
    if ( (boot_cpu_data.x86_vendor & (X86_VENDOR_AMD | X86_VENDOR_HYGON)) &&
         raw_cpuid_policy.basic.sep )
        __set_bit(X86_FEATURE_SEP, hvm_featureset);

    /*
     * VIRT_SSBD is exposed in the default policy as a result of
     * VIRT_SC_MSR_HVM being set, it also needs exposing in the max policy.
     */
    if ( boot_cpu_has(X86_FEATURE_VIRT_SC_MSR_HVM) )
        __set_bit(X86_FEATURE_VIRT_SSBD, hvm_featureset);

    /*
     * If Xen isn't virtualising MSR_SPEC_CTRL for HVM guests (functional
     * availability, or admin choice), hide the feature.
     */
    if ( !boot_cpu_has(X86_FEATURE_SC_MSR_HVM) )
    {
        __clear_bit(X86_FEATURE_IBRSB, hvm_featureset);
        __clear_bit(X86_FEATURE_IBRS, hvm_featureset);
    }
    else if ( boot_cpu_has(X86_FEATURE_AMD_SSBD) )
        /*
         * If SPEC_CTRL.SSBD is available VIRT_SPEC_CTRL.SSBD can be exposed
         * and implemented using the former. Expose in the max policy only as
         * the preference is for guests to use SPEC_CTRL.SSBD if available.
         */
        __set_bit(X86_FEATURE_VIRT_SSBD, hvm_featureset);

    /*
     * With VT-x, some features are only supported by Xen if dedicated
     * hardware support is also available.
     */
    if ( cpu_has_vmx )
    {
        if ( !cpu_has_vmx_mpx )
            __clear_bit(X86_FEATURE_MPX, hvm_featureset);

        if ( !cpu_has_vmx_xsaves )
            __clear_bit(X86_FEATURE_XSAVES, hvm_featureset);
    }

    guest_common_feature_adjustments(hvm_featureset);

    sanitise_featureset(hvm_featureset);
    cpuid_featureset_to_policy(hvm_featureset, p);
    recalculate_xstate(p);
}

static void __init calculate_hvm_def_policy(void)
{
    struct cpuid_policy *p = &hvm_def_cpuid_policy;
    uint32_t hvm_featureset[FSCAPINTS];
    unsigned int i;
    const uint32_t *hvm_featuremask;

    *p = hvm_max_cpuid_policy;
    cpuid_policy_to_featureset(p, hvm_featureset);

    hvm_featuremask = hvm_hap_supported() ?
        hvm_hap_def_featuremask : hvm_shadow_def_featuremask;

    for ( i = 0; i < ARRAY_SIZE(hvm_featureset); ++i )
        hvm_featureset[i] &= hvm_featuremask[i];

    guest_common_feature_adjustments(hvm_featureset);
    guest_common_default_feature_adjustments(hvm_featureset);

    /*
     * Only expose VIRT_SSBD if AMD_SSBD is not available, and thus
     * VIRT_SC_MSR_HVM is set.
     */
    if ( boot_cpu_has(X86_FEATURE_VIRT_SC_MSR_HVM) )
        __set_bit(X86_FEATURE_VIRT_SSBD, hvm_featureset);

    sanitise_featureset(hvm_featureset);
    cpuid_featureset_to_policy(hvm_featureset, p);
    recalculate_xstate(p);
}

void __init init_guest_cpuid(void)
{
    calculate_raw_policy();
    calculate_host_policy();

    if ( IS_ENABLED(CONFIG_PV) )
    {
        calculate_pv_max_policy();
        calculate_pv_def_policy();
    }

    if ( hvm_enabled )
    {
        calculate_hvm_max_policy();
        calculate_hvm_def_policy();
    }
}

bool recheck_cpu_features(unsigned int cpu)
{
    bool okay = true;
    struct cpuinfo_x86 c = {0};
    const struct cpuinfo_x86 *bsp = &boot_cpu_data;
    unsigned int i;

    identify_cpu(&c);

    for ( i = 0; i < NCAPINTS; ++i )
    {
        if ( !(~c.x86_capability[i] & bsp->x86_capability[i]) )
            continue;

        printk(XENLOG_ERR "CPU%u: cap[%2u] is %08x (expected %08x)\n",
               cpu, i, c.x86_capability[i], bsp->x86_capability[i]);
        okay = false;
    }

    return okay;
}

void recalculate_cpuid_policy(struct domain *d)
{
    struct cpuid_policy *p = d->arch.cpuid;
    const struct cpuid_policy *max = is_pv_domain(d)
        ? (IS_ENABLED(CONFIG_PV)  ?  &pv_max_cpuid_policy : NULL)
        : (IS_ENABLED(CONFIG_HVM) ? &hvm_max_cpuid_policy : NULL);
    uint32_t fs[FSCAPINTS], max_fs[FSCAPINTS];
    unsigned int i;

    if ( !max )
    {
        ASSERT_UNREACHABLE();
        return;
    }

    p->x86_vendor = x86_cpuid_lookup_vendor(
        p->basic.vendor_ebx, p->basic.vendor_ecx, p->basic.vendor_edx);

    p->basic.max_leaf   = min(p->basic.max_leaf,   max->basic.max_leaf);
    p->feat.max_subleaf = min(p->feat.max_subleaf, max->feat.max_subleaf);
    p->extd.max_leaf    = 0x80000000 | min(p->extd.max_leaf & 0xffff,
                                           ((p->x86_vendor & (X86_VENDOR_AMD |
                                                              X86_VENDOR_HYGON))
                                            ? CPUID_GUEST_NR_EXTD_AMD
                                            : CPUID_GUEST_NR_EXTD_INTEL) - 1);

    cpuid_policy_to_featureset(p, fs);
    cpuid_policy_to_featureset(max, max_fs);

    if ( is_hvm_domain(d) )
    {
        /*
         * HVM domains using Shadow paging have further restrictions on their
         * available paging features.
         */
        if ( !hap_enabled(d) )
        {
            for ( i = 0; i < ARRAY_SIZE(max_fs); i++ )
                max_fs[i] &= hvm_shadow_max_featuremask[i];
        }

        /* Hide nested-virt if it hasn't been explicitly configured. */
        if ( !nestedhvm_enabled(d) )
        {
            __clear_bit(X86_FEATURE_VMX, max_fs);
            __clear_bit(X86_FEATURE_SVM, max_fs);
        }
    }

    /*
     * Allow the toolstack to set HTT, X2APIC and CMP_LEGACY.  These bits
     * affect how to interpret topology information in other cpuid leaves.
     */
    __set_bit(X86_FEATURE_HTT, max_fs);
    __set_bit(X86_FEATURE_X2APIC, max_fs);
    __set_bit(X86_FEATURE_CMP_LEGACY, max_fs);

    /*
     * 32bit PV domains can't use any Long Mode features, and cannot use
     * SYSCALL on non-AMD hardware.
     */
    if ( is_pv_32bit_domain(d) )
    {
        __clear_bit(X86_FEATURE_LM, max_fs);
        if ( !(boot_cpu_data.x86_vendor & (X86_VENDOR_AMD | X86_VENDOR_HYGON)) )
            __clear_bit(X86_FEATURE_SYSCALL, max_fs);
    }

    /* Clamp the toolstacks choices to reality. */
    for ( i = 0; i < ARRAY_SIZE(fs); i++ )
        fs[i] &= max_fs[i];

    if ( p->basic.max_leaf < XSTATE_CPUID )
        __clear_bit(X86_FEATURE_XSAVE, fs);

    sanitise_featureset(fs);

    /* Fold host's FDP_EXCP_ONLY and NO_FPU_SEL into guest's view. */
    fs[FEATURESET_7b0] &= ~(cpufeat_mask(X86_FEATURE_FDP_EXCP_ONLY) |
                            cpufeat_mask(X86_FEATURE_NO_FPU_SEL));
    fs[FEATURESET_7b0] |= (host_cpuid_policy.feat._7b0 &
                           (cpufeat_mask(X86_FEATURE_FDP_EXCP_ONLY) |
                            cpufeat_mask(X86_FEATURE_NO_FPU_SEL)));

    cpuid_featureset_to_policy(fs, p);

    /* Pass host cacheline size through to guests. */
    p->basic.clflush_size = max->basic.clflush_size;

    p->extd.maxphysaddr = min(p->extd.maxphysaddr, max->extd.maxphysaddr);
    p->extd.maxphysaddr = min_t(uint8_t, p->extd.maxphysaddr,
                                paging_max_paddr_bits(d));
    p->extd.maxphysaddr = max_t(uint8_t, p->extd.maxphysaddr,
                                (p->basic.pae || p->basic.pse36) ? 36 : 32);

    p->extd.maxlinaddr = p->extd.lm ? 48 : 32;

    recalculate_xstate(p);
    recalculate_misc(p);

    for ( i = 0; i < ARRAY_SIZE(p->cache.raw); ++i )
    {
        if ( p->cache.subleaf[i].type >= 1 &&
             p->cache.subleaf[i].type <= 3 )
        {
            /* Subleaf has a valid cache type. Zero reserved fields. */
            p->cache.raw[i].a &= 0xffffc3ffu;
            p->cache.raw[i].d &= 0x00000007u;
        }
        else
        {
            /* Subleaf is not valid.  Zero the rest of the union. */
            zero_leaves(p->cache.raw, i, ARRAY_SIZE(p->cache.raw) - 1);
            break;
        }
    }

    if ( vpmu_mode == XENPMU_MODE_OFF ||
         ((vpmu_mode & XENPMU_MODE_ALL) && !is_hardware_domain(d)) )
        p->basic.raw[0xa] = EMPTY_LEAF;

    if ( !p->extd.svm )
        p->extd.raw[0xa] = EMPTY_LEAF;

    if ( !p->extd.page1gb )
        p->extd.raw[0x19] = EMPTY_LEAF;
}

int init_domain_cpuid_policy(struct domain *d)
{
    struct cpuid_policy *p = is_pv_domain(d)
        ? (IS_ENABLED(CONFIG_PV)  ?  &pv_def_cpuid_policy : NULL)
        : (IS_ENABLED(CONFIG_HVM) ? &hvm_def_cpuid_policy : NULL);

    if ( !p )
    {
        ASSERT_UNREACHABLE();
        return -EOPNOTSUPP;
    }

    p = xmemdup(p);
    if ( !p )
        return -ENOMEM;

    d->arch.cpuid = p;

    recalculate_cpuid_policy(d);

    return 0;
}

void __init init_dom0_cpuid_policy(struct domain *d)
{
    struct cpuid_policy *p = d->arch.cpuid;

    /* dom0 can't migrate.  Give it ITSC if available. */
    if ( cpu_has_itsc )
        p->extd.itsc = true;

    /*
     * Expose the "hardware speculation behaviour" bits of ARCH_CAPS to dom0,
     * so dom0 can turn off workarounds as appropriate.  Temporary, until the
     * domain policy logic gains a better understanding of MSRs.
     */
    if ( cpu_has_arch_caps )
        p->feat.arch_caps = true;

    /* Apply dom0-cpuid= command line settings, if provided. */
    if ( dom0_cpuid_cmdline )
    {
        uint32_t fs[FSCAPINTS];
        unsigned int i;

        cpuid_policy_to_featureset(p, fs);

        for ( i = 0; i < ARRAY_SIZE(fs); ++i )
        {
            fs[i] |=  dom0_enable_feat [i];
            fs[i] &= ~dom0_disable_feat[i];
        }

        cpuid_featureset_to_policy(fs, p);

        recalculate_cpuid_policy(d);
    }
}

void guest_cpuid(const struct vcpu *v, uint32_t leaf,
                 uint32_t subleaf, struct cpuid_leaf *res)
{
    const struct domain *d = v->domain;
    const struct cpuid_policy *p = d->arch.cpuid;

    *res = EMPTY_LEAF;

    /*
     * First pass:
     * - Perform max_leaf/subleaf calculations.  Out-of-range leaves return
     *   all zeros, following the AMD model.
     * - Fill in *res with static data.
     * - Dispatch the virtualised leaves to their respective handlers.
     */
    switch ( leaf )
    {
    case 0 ... CPUID_GUEST_NR_BASIC - 1:
        ASSERT(p->basic.max_leaf < ARRAY_SIZE(p->basic.raw));
        if ( leaf > min_t(uint32_t, p->basic.max_leaf,
                          ARRAY_SIZE(p->basic.raw) - 1) )
            return;

        switch ( leaf )
        {
        case 0x4:
            if ( subleaf >= ARRAY_SIZE(p->cache.raw) )
                return;

            *res = array_access_nospec(p->cache.raw, subleaf);
            break;

        case 0x7:
            ASSERT(p->feat.max_subleaf < ARRAY_SIZE(p->feat.raw));
            if ( subleaf > min_t(uint32_t, p->feat.max_subleaf,
                                 ARRAY_SIZE(p->feat.raw) - 1) )
                return;

            *res = array_access_nospec(p->feat.raw, subleaf);
            break;

        case 0xb:
            if ( subleaf >= ARRAY_SIZE(p->topo.raw) )
                return;

            *res = array_access_nospec(p->topo.raw, subleaf);
            break;

        case XSTATE_CPUID:
            if ( !p->basic.xsave || subleaf >= ARRAY_SIZE(p->xstate.raw) )
                return;

            *res = array_access_nospec(p->xstate.raw, subleaf);
            break;

        default:
            *res = array_access_nospec(p->basic.raw, leaf);
            break;
        }
        break;

    case 0x40000000 ... 0x400000ff:
        if ( is_viridian_domain(d) )
            return cpuid_viridian_leaves(v, leaf, subleaf, res);

        /*
         * Fallthrough.
         *
         * Intel reserve up until 0x4fffffff for hypervisor use.  AMD reserve
         * only until 0x400000ff, but we already use double that.
         */
    case 0x40000100 ... 0x400001ff:
        return cpuid_hypervisor_leaves(v, leaf, subleaf, res);

    case 0x80000000 ... 0x80000000 + CPUID_GUEST_NR_EXTD - 1:
        ASSERT((p->extd.max_leaf & 0xffff) < ARRAY_SIZE(p->extd.raw));
        if ( (leaf & 0xffff) > min_t(uint32_t, p->extd.max_leaf & 0xffff,
                                     ARRAY_SIZE(p->extd.raw) - 1) )
            return;

        *res = array_access_nospec(p->extd.raw, leaf & 0xffff);
        break;

    default:
        return;
    }

    /*
     * Skip dynamic adjustments if we are in the wrong context.
     *
     * All dynamic adjustments depends on current register state, which will
     * be stale if the vcpu is running elsewhere.  It is simpler, quicker, and
     * more reliable for the caller to do nothing (consistently) than to hand
     * back stale data which it can't use safely.
     */
    if ( v != current )
        return;

    /*
     * Second pass:
     * - Dynamic adjustments
     */
    switch ( leaf )
    {
        const struct cpu_user_regs *regs;

    case 0x1:
        /* TODO: Rework topology logic. */
        res->b &= 0x00ffffffu;
        if ( is_hvm_domain(d) )
            res->b |= (v->vcpu_id * 2) << 24;

        /* TODO: Rework vPMU control in terms of toolstack choices. */
        if ( vpmu_available(v) &&
             vpmu_is_set(vcpu_vpmu(v), VPMU_CPU_HAS_DS) )
        {
            res->d |= cpufeat_mask(X86_FEATURE_DS);
            if ( cpu_has(&current_cpu_data, X86_FEATURE_DTES64) )
                res->c |= cpufeat_mask(X86_FEATURE_DTES64);
            if ( cpu_has(&current_cpu_data, X86_FEATURE_DSCPL) )
                res->c |= cpufeat_mask(X86_FEATURE_DSCPL);
        }

        if ( is_hvm_domain(d) )
        {
            /* OSXSAVE clear in policy.  Fast-forward CR4 back in. */
            if ( v->arch.hvm.guest_cr[4] & X86_CR4_OSXSAVE )
                res->c |= cpufeat_mask(X86_FEATURE_OSXSAVE);
        }
        else /* PV domain */
        {
            regs = guest_cpu_user_regs();

            /*
             * !!! OSXSAVE handling for PV guests is non-architectural !!!
             *
             * Architecturally, the correct code here is simply:
             *
             *   if ( v->arch.pv.ctrlreg[4] & X86_CR4_OSXSAVE )
             *       c |= cpufeat_mask(X86_FEATURE_OSXSAVE);
             *
             * However because of bugs in Xen (before c/s bd19080b, Nov 2010,
             * the XSAVE cpuid flag leaked into guests despite the feature not
             * being available for use), buggy workarounds where introduced to
             * Linux (c/s 947ccf9c, also Nov 2010) which relied on the fact
             * that Xen also incorrectly leaked OSXSAVE into the guest.
             *
             * Furthermore, providing architectural OSXSAVE behaviour to a
             * many Linux PV guests triggered a further kernel bug when the
             * fpu code observes that XSAVEOPT is available, assumes that
             * xsave state had been set up for the task, and follows a wild
             * pointer.
             *
             * Older Linux PVOPS kernels however do require architectural
             * behaviour.  They observe Xen's leaked OSXSAVE and assume they
             * can already use XSETBV, dying with a #UD because the shadowed
             * CR4.OSXSAVE is clear.  This behaviour has been adjusted in all
             * observed cases via stable backports of the above changeset.
             *
             * Therefore, the leaking of Xen's OSXSAVE setting has become a
             * defacto part of the PV ABI and can't reasonably be corrected.
             * It can however be restricted to only the enlightened CPUID
             * view, as seen by the guest kernel.
             *
             * The following situations and logic now applies:
             *
             * - Hardware without CPUID faulting support and native CPUID:
             *    There is nothing Xen can do here.  The hosts XSAVE flag will
             *    leak through and Xen's OSXSAVE choice will leak through.
             *
             *    In the case that the guest kernel has not set up OSXSAVE, only
             *    SSE will be set in xcr0, and guest userspace can't do too much
             *    damage itself.
             *
             * - Enlightened CPUID or CPUID faulting available:
             *    Xen can fully control what is seen here.  When the guest has
             *    been configured to have XSAVE available, guest kernels need
             *    to see the leaked OSXSAVE via the enlightened path, but
             *    guest userspace and the native is given architectural
             *    behaviour.
             *
             *    Emulated vs Faulted CPUID is distinguised based on whether a
             *    #UD or #GP is currently being serviced.
             */
            /* OSXSAVE clear in policy.  Fast-forward CR4 back in. */
            if ( (v->arch.pv.ctrlreg[4] & X86_CR4_OSXSAVE) ||
                 (p->basic.xsave &&
                  regs->entry_vector == TRAP_invalid_op &&
                  guest_kernel_mode(v, regs) &&
                  (read_cr4() & X86_CR4_OSXSAVE)) )
                res->c |= cpufeat_mask(X86_FEATURE_OSXSAVE);

            /*
             * At the time of writing, a PV domain is the only viable option
             * for Dom0.  Several interactions between dom0 and Xen for real
             * hardware setup have unfortunately been implemented based on
             * state which incorrectly leaked into dom0.
             *
             * These leaks are retained for backwards compatibility, but
             * restricted to the hardware domains kernel only.
             */
            if ( is_hardware_domain(d) && guest_kernel_mode(v, regs) )
            {
                /*
                 * MONITOR never leaked into PV guests, as PV guests cannot
                 * use the MONITOR/MWAIT instructions.  As such, they require
                 * the feature to not being present in emulated CPUID.
                 *
                 * Modern PVOPS Linux try to be cunning and use native CPUID
                 * to see if the hardware actually supports MONITOR, and by
                 * extension, deep C states.
                 *
                 * If the feature is seen, deep-C state information is
                 * obtained from the DSDT and handed back to Xen via the
                 * XENPF_set_processor_pminfo hypercall.
                 *
                 * This mechanism is incompatible with an HVM-based hardware
                 * domain, and also with CPUID Faulting.
                 *
                 * Luckily, Xen can be just as 'cunning', and distinguish an
                 * emulated CPUID from a faulted CPUID by whether a #UD or #GP
                 * fault is currently being serviced.  Yuck...
                 */
                if ( cpu_has_monitor && regs->entry_vector == TRAP_gp_fault )
                    res->c |= cpufeat_mask(X86_FEATURE_MONITOR);

                /*
                 * While MONITOR never leaked into PV guests, EIST always used
                 * to.
                 *
                 * Modern PVOPS Linux will only parse P state information from
                 * the DSDT and return it to Xen if EIST is seen in the
                 * emulated CPUID information.
                 */
                if ( cpu_has_eist )
                    res->c |= cpufeat_mask(X86_FEATURE_EIST);
            }
        }
        goto common_leaf1_adjustments;

    case 0x5:
        /*
         * Leak the hardware MONITOR leaf under the same conditions that the
         * MONITOR feature flag is leaked.  See above for details.
         */
        regs = guest_cpu_user_regs();
        if ( is_pv_domain(d) && is_hardware_domain(d) &&
             guest_kernel_mode(v, regs) && cpu_has_monitor &&
             regs->entry_vector == TRAP_gp_fault )
            *res = raw_cpuid_policy.basic.raw[5];
        break;

    case 0x7:
        switch ( subleaf )
        {
        case 0:
            /* OSPKE clear in policy.  Fast-forward CR4 back in. */
            if ( (is_pv_domain(d)
                  ? v->arch.pv.ctrlreg[4]
                  : v->arch.hvm.guest_cr[4]) & X86_CR4_PKE )
                res->c |= cpufeat_mask(X86_FEATURE_OSPKE);
            break;
        }
        break;

    case 0xa:
        /* TODO: Rework vPMU control in terms of toolstack choices. */
        if ( boot_cpu_data.x86_vendor != X86_VENDOR_INTEL ||
             !vpmu_available(v) )
            *res = EMPTY_LEAF;
        else
        {
            struct {
                uint8_t version;
                uint8_t general_nr;
                uint8_t general_width;
                uint8_t arch_nr;
            } eax;

            BUILD_BUG_ON(sizeof(eax) == sizeof(res->a));
            memcpy(&eax, &res->a, sizeof(eax));

            /* Report at most VPMU_VERSION_MAX since that's all we currently emulate. */
            if ( eax.version >  VPMU_VERSION_MAX) {
                gdprintk(XENLOG_WARNING, "Limiting PMU version to %d (actual %d)", VPMU_VERSION_MAX, eax.version);
                eax.version = VPMU_VERSION_MAX;
            }

            if ( eax.general_nr > MSR_P6_PERFCTR_MAX) {
                gdprintk(XENLOG_WARNING "Limiting general purpose PMU count to %d (actual %d)", MSR_P6_PERFCTR_MAX, eax.general_nr);
                eax.general_nr = MSR_P6_PERFCTR_MAX;
            }

            if ( vpmu.features & (XENPMU_FEATURE_IPC_ONLY |
                                 XENPMU_FEATURE_ARCH_ONLY) ) {
                unsigned limit = ( vpmu.features & XENPMU_FEATURE_ARCH_ONLY ) ? VPMU_ARCH_EVENTS_MAX : VPMU_IPC_EVENTS_MAX;
                if (limit < eax.arch_nr) {
                    gdprintk(XENLOG_WARNING "Limiting architectural PMU events to %d (actual %d)", eax.arch_nr);
                    eax.arch_nr = limit;
                }
            }
            memcpy(&res->a, &eax, sizeof(res->a));

        }
        break;

    case 0xb:
        /*
         * In principle, this leaf is Intel-only.  In practice, it is tightly
         * coupled with x2apic, and we offer an x2apic-capable APIC emulation
         * to guests on AMD hardware as well.
         *
         * TODO: Rework topology logic.
         */
        if ( p->basic.x2apic )
        {
            *(uint8_t *)&res->c = subleaf;

            /* Fix the x2APIC identifier. */
            res->d = v->vcpu_id * 2;
        }
        break;

    case XSTATE_CPUID:
        switch ( subleaf )
        {
        case 1:
            if ( p->xstate.xsavec || p->xstate.xsaves )
            {
                /*
                 * TODO: Figure out what to do for XSS state.  VT-x manages
                 * host vs guest MSR_XSS automatically, so as soon as we start
                 * supporting any XSS states, the wrong XSS will be in
                 * context.
                 */
                BUILD_BUG_ON(XSTATE_XSAVES_ONLY != 0);

                /*
                 * Read CPUID[0xD,0/1].EBX from hardware.  They vary with
                 * enabled XSTATE, and appropraite XCR0|XSS are in context.
                 */
        case 0:
                res->b = cpuid_count_ebx(leaf, subleaf);
            }
            break;
        }
        break;

    case 0x80000001:
        /* SYSCALL is hidden outside of long mode on Intel. */
        if ( p->x86_vendor == X86_VENDOR_INTEL &&
             is_hvm_domain(d) && !hvm_long_mode_active(v) )
            res->d &= ~cpufeat_mask(X86_FEATURE_SYSCALL);

    common_leaf1_adjustments:
        if ( is_hvm_domain(d) )
        {
            /* Fast-forward MSR_APIC_BASE.EN. */
            if ( vlapic_hw_disabled(vcpu_vlapic(v)) )
                res->d &= ~cpufeat_mask(X86_FEATURE_APIC);

            /*
             * PSE36 is not supported in shadow mode.  This bit should be
             * clear in hvm_shadow_max_featuremask[].
             *
             * However, an unspecified version of Hyper-V from 2011 refuses to
             * start as the "cpu does not provide required hw features" if it
             * can't see PSE36.
             *
             * As a workaround, leak the toolstack-provided PSE36 value into a
             * shadow guest if the guest is already using PAE paging (and
             * won't care about reverting back to PSE paging).  Otherwise,
             * knoble it, so a 32bit guest doesn't get the impression that it
             * could try to use PSE36 paging.
             */
            if ( !hap_enabled(d) && !hvm_pae_enabled(v) )
                res->d &= ~cpufeat_mask(X86_FEATURE_PSE36);
        }
        else /* PV domain */
        {
            /*
             * MTRR used to unconditionally leak into PV guests.  They cannot
             * MTRR infrastructure at all, and shouldn't be able to see the
             * feature.
             *
             * Modern PVOPS Linux self-clobbers the MTRR feature, to avoid
             * trying to use the associated MSRs.  Xenolinux-based PV dom0's
             * however use the MTRR feature as an indication of the presence
             * of the XENPF_{add,del,read}_memtype hypercalls.
             */
            if ( is_hardware_domain(d) && cpu_has_mtrr &&
                 guest_kernel_mode(v, guest_cpu_user_regs()) )
                res->d |= cpufeat_mask(X86_FEATURE_MTRR);
        }
        break;
    }
}

static void __init __maybe_unused build_assertions(void)
{
    BUILD_BUG_ON(ARRAY_SIZE(known_features) != FSCAPINTS);
    BUILD_BUG_ON(ARRAY_SIZE(pv_max_featuremask) != FSCAPINTS);
    BUILD_BUG_ON(ARRAY_SIZE(hvm_shadow_max_featuremask) != FSCAPINTS);
    BUILD_BUG_ON(ARRAY_SIZE(hvm_hap_max_featuremask) != FSCAPINTS);
    BUILD_BUG_ON(ARRAY_SIZE(deep_features) != FSCAPINTS);

    /* Find some more clever allocation scheme if this trips. */
    BUILD_BUG_ON(sizeof(struct cpuid_policy) > PAGE_SIZE);

    BUILD_BUG_ON(sizeof(raw_cpuid_policy.basic) !=
                 sizeof(raw_cpuid_policy.basic.raw));
    BUILD_BUG_ON(sizeof(raw_cpuid_policy.feat) !=
                 sizeof(raw_cpuid_policy.feat.raw));
    BUILD_BUG_ON(sizeof(raw_cpuid_policy.xstate) !=
                 sizeof(raw_cpuid_policy.xstate.raw));
    BUILD_BUG_ON(sizeof(raw_cpuid_policy.extd) !=
                 sizeof(raw_cpuid_policy.extd.raw));
}

/*
 * Local variables:
 * mode: C
 * c-file-style: "BSD"
 * c-basic-offset: 4
 * tab-width: 4
 * indent-tabs-mode: nil
 * End:
 */

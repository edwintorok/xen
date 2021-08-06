/*
 * vpmu.h: PMU virtualization for HVM domain.
 *
 * Copyright (c) 2007, Intel Corporation.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms and conditions of the GNU General Public License,
 * version 2, as published by the Free Software Foundation.
 *
 * This program is distributed in the hope it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Haitao Shan <haitao.shan@intel.com>
 */

#ifndef __ASM_X86_HVM_VPMU_H_
#define __ASM_X86_HVM_VPMU_H_

#include <public/pmu.h>

#define vcpu_vpmu(vcpu)   (&(vcpu)->arch.vpmu)
#define vpmu_vcpu(vpmu)   container_of((vpmu), struct vcpu, arch.vpmu)
#define vpmu_available(vcpu) vpmu_is_set(vcpu_vpmu(vcpu), VPMU_AVAILABLE)

#define MSR_TYPE_COUNTER            0
#define MSR_TYPE_CTRL               1
#define MSR_TYPE_GLOBAL             2
#define MSR_TYPE_ARCH_COUNTER       3
#define MSR_TYPE_ARCH_CTRL          4

/* Start of PMU register bank */
#define vpmu_reg_pointer(ctxt, offset) ((void *)((uintptr_t)ctxt + \
                                                 (uintptr_t)ctxt->offset))

/* Arch specific operations shared by all vpmus */
struct arch_vpmu_ops {
    int (*initialise)(struct vcpu *v);
    int (*do_wrmsr)(unsigned int msr, uint64_t msr_content);
    int (*do_rdmsr)(unsigned int msr, uint64_t *msr_content);
    int (*do_interrupt)(struct cpu_user_regs *regs);
    void (*arch_vpmu_destroy)(struct vcpu *v);
    int (*arch_vpmu_save)(struct vcpu *v, bool_t to_guest);
    int (*arch_vpmu_load)(struct vcpu *v, bool_t from_guest);
    void (*arch_vpmu_dump)(const struct vcpu *);

#ifdef CONFIG_MEM_SHARING
    int (*allocate_context)(struct vcpu *v);
#endif
};

const struct arch_vpmu_ops *core2_vpmu_init(void);
const struct arch_vpmu_ops *amd_vpmu_init(void);
const struct arch_vpmu_ops *hygon_vpmu_init(void);

struct vpmu_struct {
    u32 flags;
    u32 last_pcpu;
    u32 hw_lapic_lvtpc;
    void *context;      /* May be shared with PV guest */
    void *priv_context; /* hypervisor-only */
    size_t context_size;
    size_t priv_context_size;
    struct xen_pmu_data *xenpmu_data;
    spinlock_t vpmu_lock;
};

/* VPMU states */
#define VPMU_INITIALIZED                    0x0001
#define VPMU_CONTEXT_ALLOCATED              0x0002
#define VPMU_CONTEXT_LOADED                 0x0004
#define VPMU_RUNNING                        0x0008
#define VPMU_CONTEXT_SAVE                   0x0010  /* Force context save */
#define VPMU_FROZEN                         0x0020  /* Stop counters while VCPU is not running */
#define VPMU_PASSIVE_DOMAIN_ALLOCATED       0x0040
/* PV(H) guests: VPMU registers are accessed by guest from shared page */
#define VPMU_CACHED                         0x0080
#define VPMU_AVAILABLE                      0x0100

/* Intel-specific VPMU features */
#define VPMU_CPU_HAS_DS                     0x1000 /* Has Debug Store */
#define VPMU_CPU_HAS_BTS                    0x2000 /* Has Branch Trace Store */

#define VPMU_VERSION_MAX                    0x5

static inline void vpmu_set(struct vpmu_struct *vpmu, const u32 mask)
{
    vpmu->flags |= mask;
}
static inline void vpmu_reset(struct vpmu_struct *vpmu, const u32 mask)
{
    vpmu->flags &= ~mask;
}
static inline void vpmu_clear(struct vpmu_struct *vpmu)
{
    /* VPMU_AVAILABLE should be altered by get/put_vpmu(). */
    vpmu->flags &= VPMU_AVAILABLE;
}
static inline bool_t vpmu_is_set(const struct vpmu_struct *vpmu, const u32 mask)
{
    return !!(vpmu->flags & mask);
}
static inline bool_t vpmu_are_all_set(const struct vpmu_struct *vpmu,
                                      const u32 mask)
{
    return !!((vpmu->flags & mask) == mask);
}

void vpmu_lvtpc_update(uint32_t val);
int vpmu_do_msr(unsigned int msr, uint64_t *msr_content, bool is_write);
void vpmu_do_interrupt(struct cpu_user_regs *regs);
void vpmu_initialise(struct vcpu *v);
void vpmu_destroy(struct vcpu *v);
void vpmu_save(struct vcpu *v);
void cf_check vpmu_save_force(void *arg);
int vpmu_load(struct vcpu *v, bool_t from_guest);
void vpmu_dump(struct vcpu *v);

static inline int vpmu_do_wrmsr(unsigned int msr, uint64_t msr_content)
{
    return vpmu_do_msr(msr, &msr_content, true /* write */);
}
static inline int vpmu_do_rdmsr(unsigned int msr, uint64_t *msr_content)
{
    return vpmu_do_msr(msr, msr_content, false /* read */);
}

extern unsigned int vpmu_mode;
extern unsigned int vpmu_features;

/* Context switch */
static inline void vpmu_switch_from(struct vcpu *prev)
{
    if ( vpmu_mode & (XENPMU_MODE_SELF | XENPMU_MODE_HV) )
        vpmu_save(prev);
}

static inline void vpmu_switch_to(struct vcpu *next)
{
    if ( vpmu_mode & (XENPMU_MODE_SELF | XENPMU_MODE_HV) )
        vpmu_load(next, 0);
}

#ifdef CONFIG_MEM_SHARING
int vpmu_allocate_context(struct vcpu *v);
#else
static inline int vpmu_allocate_context(struct vcpu *v)
{
    ASSERT_UNREACHABLE();
    return 0;
}
#endif

/*
 * See the Pre-Defined Architectural Performance Events table
 * from the Intel 64 and IA-32 Architectures Software
 * Developer's Manual, Volume 3B, System Programming Guide,
 * Part 2.
 */
#define VPMU_IPC_EVENTS(DEF) \
    DEF(0x003c)	/* UnHalted Core Cycles */\
    DEF(0x013c)	/* UnHalted Reference Cycles */\
    DEF(0x00c0)	/* Instructions Retired */\


#define VPMU_ARCH_EVENTS(DEF) \
    VPMU_IPC_EVENTS(DEF)\
    DEF(0x4f2e)	/* Last Level Cache References */\
    DEF(0x412e)	/* Last Level Cache Misses */\
    DEF(0x00c4)	/* Branch Instructions Retired */\
    DEF(0x00c5)	/* All Branch Mispredict Retired */\
    DEF(0x01a4)	/* Topdown Slots */\

#define DEFCASE(x) case (x):
#define DEFSUM(x) +1
#define DEFCOUNT(X) (0+X(DEFSUM))

#define VPMU_IPC_EVENTS_MAX DEFCOUNT(VPMU_IPC_EVENTS)
#define VPMU_ARCH_EVENTS_MAX DEFCOUNT(VPMU_ARCH_EVENTS)

#endif /* __ASM_X86_HVM_VPMU_H_*/


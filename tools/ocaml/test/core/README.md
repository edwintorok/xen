Design principles:

* a minimal test suite to check some minimal properties of oxenstored, and
its memory usage security fix. 
A more comprehensive suite is available in `../ext`.

* Compatible with OCaml 4.02.3+

    * when functionality from newer versions is needed use `dune`'s support for
    conditional compilation (`select`) to have minimal fallback code

* Depends only on libraries shipped with OCaml

    * when libraries that are shipped with newer OCaml versions are needed,
    use the above method for providing fallback code for the functionality used
    from those libraries

    * other libraries can enhance the functionality here, and can be chosen using
    dune `select` for executables, or `optional` for libraries

    * it is recommended to keep the `select`-ed files a short one-liner,
    just including the module built as an optional library: this ensures that
    other variants get compile-tested regularly too

    * internal libraries that in their entirety only function when external
    libraries are present should be in `../ext` instead

    * tests should use `select` to chose a fallback implementation that prints
    a SKIP message when dependencies are not present, this way a `dune runtest`
    would not fail with build errors

    * motivation: avoid circular build dependencies when building Xen and
    packages that depend on it by having a mode with minimal dependencies that
    can break such cycles.

    * the presence of `dune` is always required though (e.g. from system package manager),
    otherwise it would require maintaining 2 build systems, and Xen's Makefiles
    aren't usable for development (they are not incremental and lack LSP support).
    For now version 2.1, although that may be bumped to 2.7 in the future
    after updating the CI.

    * Dune virtual libraries are not used because they prevent inlining from working,
    and they may require a newer version of Dune

* Be careful that the test code may or may not run in a Xen VM, do not
start using its xenstore by default, unless explicitly requested on the cmdline

* Runs correctly on 32-bit, 64-bit, bytecode, unikernel and native targets.
This means that there should be at least one `select` target that doesn't require
`unix` (transitively)

* optimized for native 64-bit targets (not necessarily just x86-64)

* prefix library names with `tracer_` to avoid clashes where appropriate

* code called during the main testsuite avoids allocations in the major heap
where possible (to avoid interfering with measuring memory usage).
In dev profile builds some temporary values may be allocated in the minor heap
due to the lack of inlining.

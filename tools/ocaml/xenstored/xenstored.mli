(* this file is empty on purpose.
   Dune 2.9+ would support an 'executables_implicit_empty_intf'.

   Benefits from the manual:

    By default, executables defined via (executables(s) ...) or (test(s) ...)
  stanzas are compiled with the interface file provided (e.g., .mli or rei).
  Since these modules cannot be used as library dependencies, it’s common to
  give them empty interface files to strengthen the compiler’s ability to
  detect unused values in these modules.
*)

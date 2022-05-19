open Monolith
type 'a property = 'a -> bool

type 'a ensures = 'a -> 'a diagnostic
type 'a raises = ('a, exn) result -> ('a, exn) result diagnostic

type 'a requires = 'a property

type 'a equivalent = ('a, exn) result -> ('a, exn) result

type 'a consumes = 'a -> unit (* adds to ephemeron map that is checked in requires? *)

type 'a invariant = 'a -> ('a -> unit) code
(* goes into declare_abstract_type *)
 
val requires: ?loc:string -> 'a property -> 'a requires
val ensures: ?loc:string -> 'a property ->  'a ensures
val raises: ?loc:string -> ('a -> exn) property -> 'a raises
val checks: ?loc:string -> 'a property -> 'a raises

val equivalent: ?loc:string -> ('a -> 'b) -> 'b equivalent

val consumes: ?loc:string -> 'a -> 'a consumes

val invariant: 'a property -> 'a invariant

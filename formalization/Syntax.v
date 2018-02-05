(********************)
(********************)
(****            ****)
(****   Syntax   ****)
(****            ****)
(********************)
(********************)

Require Import Main.Name.

(* Terms (e) *)

Inductive term : Type :=
| eVar : name -> term (* Metavariable: x *)
| eAbs : name -> type -> row -> term -> term
| eApp : term -> term -> term
| eTAbs : name -> term -> term
| eTApp : term -> type -> term
| eEffect : name -> name -> type -> row -> term -> term
| eHandle : name -> term -> term -> term
| eDo : name -> term -> term -> term

(* Proper types (t) *)

with type : Type :=
| tVar : name -> type (* Metavariable: a *)
| tArrow : type -> row -> type -> row -> type
| tForAll : name -> type -> row -> type

(* Rows (r) *)

with row : Type :=
| rEmpty : row
| rSingleton : name -> row
| rUnion : row -> row -> row

(* Type contexts (c) *)

with context : Type :=
| cEmpty : context
| cTExtend : context -> name -> type -> row -> context
| cKExtend : context -> name -> context

(* Effect map (em) *)

with effectMap : Type :=
| emEmpty : effectMap
| emExtend : effectMap -> name -> type -> row -> effectMap.

(********************)
(********************)
(****            ****)
(****   Syntax   ****)
(****            ****)
(********************)
(********************)

Require Import Main.Name.

Import Name.

(* Terms (e) *)

Inductive term : Type :=
| eVar : eName -> term (* Metavariable: x *)
| eAbs : eName -> type -> row -> term -> term
| eApp : term -> term -> term
| eTAbs : tName -> term -> term
| eTApp : term -> type -> term
| eEffect : tName -> eName -> type -> row -> term -> term
| eHandle : tName -> term -> term -> term
| eDo : eName -> term -> term -> term

(* Proper types (t) *)

with type : Type :=
| tVar : tName -> type (* Metavariable: a *)
| tArrow : type -> row -> type -> row -> type
| tForAll : tName -> type -> row -> type

(* Rows (r) *)

with row : Type :=
| rEmpty : row
| rSingleton : tName -> row
| rUnion : row -> row -> row

(* Type contexts (c) *)

with context : Type :=
| cEmpty : context
| cTExtend : context -> eName -> type -> row -> context
| cKExtend : context -> tName -> context

(* Effect map (em) *)

with effectMap : Type :=
| emEmpty : effectMap
| emExtend : effectMap -> tName -> type -> row -> effectMap.

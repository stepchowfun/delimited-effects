(********************)
(********************)
(****            ****)
(****   Syntax   ****)
(****            ****)
(********************)
(********************)

(* NOTE: We will use De Bruijn indices instead of variable names. *)

(* Terms (e) *)

Inductive term : Type :=
| eVar : nat -> term (* Metavariable: x *)
| eAbs : type -> row -> term -> term
| eApp : term -> term -> term
| eTAbs : term -> term
| eTApp : term -> type -> term
| eEffect : type -> row -> term -> term
| eHandle : nat -> term -> term -> term
| eDo : term -> term -> term

(* Proper types (t) *)

with type : Type :=
| tVar : nat -> type (* Metavariable: a *)
| tArrow : type -> row -> type -> row -> type
| tForall : type -> row -> type

(* Rows (r) *)

with row : Type :=
| rEmpty : row
| rSingleton : nat -> row
| rUnion : row -> row -> row

(* Type contexts (c) *)

with context : Type :=
| cEmpty : context
| cTExtend : context -> nat -> type -> row -> context
| cKExtend : context -> nat -> context

(* Effect map (em) *)

with effectMap : Type :=
| emEmpty : effectMap
| emExtend : effectMap -> nat -> type -> row -> effectMap.

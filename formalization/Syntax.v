(* NOTE: We will use De Bruijn indices instead of variable names. *)

(* Terms *)

Inductive term : Type :=
| eVar : nat -> term
| eAbs : term -> term
| eApp : term -> term -> term
| eTAbs : term -> term
| eTApp : term -> type -> term
| eEffect : type -> row -> term -> term
| eHandle : nat -> term -> term -> term
| eAnno : term -> type -> term

(* Proper types *)

with type : Type :=
| tVar : nat -> type
| tArrow : type -> row -> type -> row -> type
| tForall : type -> row -> type

(* Rows *)

with row : Type :=
| rEmpty : row
| rSingleton : nat -> row
| rUnion : row -> row -> row

(* Type contexts *)

with context : Type :=
| cEmpty : context
| cTExtend : context -> nat -> type -> row -> context
| cKExtend : context -> nat -> context

(* Effect map *)

with effectMap : Type :=
| emEmpty : effectMap
| emExtend : effectMap -> nat -> type -> row -> effectMap.

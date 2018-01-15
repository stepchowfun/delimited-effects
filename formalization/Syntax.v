(* NOTE: We will use De Bruijn indices instead of variable names. *)

(* Terms *)

Inductive term : Type :=
| eVar : nat -> term
| eAbs : nat -> term -> term
| eApp : term -> term -> term
| eTAbs : nat -> term -> term
| eTApp : term -> type -> term
| eHandle : nat -> term -> term -> term
| eAnno : term -> type -> term

(* Proper types *)

with type : Type :=
| tArrow : type -> row -> type -> row -> type
| tForall : nat -> type -> row -> type

(* Rows *)

with row : Type :=
| rEmpty : row
| rSingleton : nat -> row
| rUnion : row -> row -> row

(* Hoisted term sets *)

with hoistedSet : Type :=
| hEmpty : hoistedSet
| hSingleton : nat -> hoistedSet
| hUnion : hoistedSet -> hoistedSet -> hoistedSet

(* Type contexts *)

with context : Type :=
| cEmpty : context
| cTExtend : context -> nat -> type -> row -> context
| cKExtend : context -> nat -> context

(* Effect map *)

with effectMap : Type :=
| emEmpty : effectMap
| emExtend : effectMap -> nat -> nat -> effectMap.

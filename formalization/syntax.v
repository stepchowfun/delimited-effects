Require Import Coq.Strings.String.

(* Identifiers *)

Module Type Identifiers.
  Parameter termId : Type.
  Parameter typeId : Type.
  Parameter rowId : Type.
  Parameter effectId : Type.
  Axiom termIdEqDec :
    forall (id1 id2 : termId), {id1 = id2} + {id1 <> id2}.
  Axiom typeIdEqDec :
    forall (id1 id2 : typeId), {id1 = id2} + {id1 <> id2}.
  Axiom effectIdEqDec :
    forall (id1 id2 : effectId), {id1 = id2} + {id1 <> id2}.
End Identifiers.

Module Syntax (IdentifiersInstance : Identifiers).
Import IdentifiersInstance.

(* Terms *)

Inductive term : Type :=
| eunit : term
| evar : termId -> term
| eabs : termId -> type -> term -> term
| eapp : term -> term -> term
| etabs : typeId -> term -> term
| etapp : term -> type -> term
| erabs : rowId -> term -> term
| erapp : term -> row -> term
| eprovide : effectId -> term -> term -> term
| ecoprovide : effectId -> term -> term -> term

(* Proper types *)

with type : Type :=
| tvar : typeId -> type
| tunit : type
| tarrow : type -> type -> row -> type
| ttforall : typeId -> type -> row -> type
| trforall : rowId -> type -> row -> type

(* Rows *)

with row : Type :=
| rvar : rowId -> row
| rempty : row
| rsingleton : effectId -> row
| runion : row -> row -> row
| rdiff : row -> row -> row.

(* Type contexts *)

Inductive context : Type :=
| cempty : context
| ceextend : context -> termId -> type -> row -> context.

(* Effect map *)

Inductive effectMap : Type :=
| emempty : effectMap
| emextend :
  effectMap ->
  effectId ->
  termId ->
  type ->
  row ->
  effectMap.

End Syntax.

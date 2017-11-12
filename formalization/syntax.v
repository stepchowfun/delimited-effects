Require Import Coq.Lists.List.

(* Identifiers *)

Module Type Identifiers.
  Parameter termId : Type.
  Parameter effectId : Type.
  Axiom termIdEqDec :
    forall (id1 id2 : termId), {id1 = id2} + {id1 <> id2}.
  Axiom effectIdEqDec :
    forall (id1 id2 : effectId), {id1 = id2} + {id1 <> id2}.
End Identifiers.

(* Syntax *)

Module Syntax (IdentifiersInstance : Identifiers).
  Import IdentifiersInstance.

  (* Terms *)

  Inductive term : Type :=
  | eUnit : term
  | eVar : termId -> term
  | eAbs : termId -> type -> term -> term
  | eApp : term -> term -> term
  | eHandle : effectId -> list effectId -> term -> term -> term

  (* Proper types *)

  with type : Type :=
  | tUnit : type
  | tArrow : type -> type -> row -> type

  (* Rows *)

  with row : Type :=
  | rEmpty : row
  | rSingleton : effectId -> row
  | rUnion : row -> row -> row
  | rDiff : row -> row -> row.

  (* Type contexts *)

  Inductive context : Type :=
  | cEmpty : context
  | cExtend : context -> termId -> type -> row -> context.

  (* Effect map *)

  Inductive effectMap : Type :=
  | emEmpty : effectMap
  | emExtend : effectMap -> effectId -> termId -> type -> row -> effectMap.
End Syntax.

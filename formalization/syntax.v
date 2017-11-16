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
  | eAbs : termId -> term -> term
  | eApp : term -> term -> term
  | eHandle : effectId -> term -> term -> term
  | eAnno : term -> type -> term

  (* Proper types *)

  with type : Type :=
  | tUnit : type
  | tArrow : type -> row -> type -> row -> type

  (* Rows *)

  with row : Type :=
  | rEmpty : row
  | rSingleton : effectId -> row
  | rUnion : row -> row -> row

  (* Variable sets *)

  with varSet : Type :=
  | vsEmpty : varSet
  | vsSingleton : termId -> varSet
  | vsUnion : varSet -> varSet -> varSet

  (* Type contexts *)

  with context : Type :=
  | cEmpty : context
  | cExtend : context -> termId -> type -> row -> context

  (* Effect map *)

  with effectMap : Type :=
  | emEmpty : effectMap
  | emExtend : effectMap -> effectId -> termId -> type -> row -> effectMap.
End Syntax.

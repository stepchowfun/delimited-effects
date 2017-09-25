Require Import Coq.Strings.String.

(* Identifiers *)

Module Type Identifiers.
  Parameter termId : Type.
  Parameter typeId : Type.
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
| etabs : typeId -> kind -> term -> term
| etapp : term -> type -> term

(* Types *)

with type : Type :=
| tvar : typeId -> type
| tembellished : type -> type -> type -> type
| tunit : type
| tarrow : type -> type -> type
| tforall : typeId -> kind -> type -> type
| tempty : type
| tsingleton : effectId -> type
| tunion : type -> type -> type
| tdiff : type -> type -> type

(* Kinds *)

with kind : Type :=
| kproper : kind
| krow : kind
| kembellished : kind.

(* Type contexts *)

Inductive context : Type :=
| cempty : context
| ceextend : context -> termId -> type -> context
| ctextend : context -> typeId -> kind -> context.

(* Effect map *)

Inductive effectMap : Type :=
| emempty : effectMap
| emextend : effectMap -> effectId -> termId -> type -> effectMap.

(* Coeffect map *)

Inductive coeffectMap : Type :=
| cmempty : coeffectMap
| cmextend : coeffectMap -> effectId -> termId -> type -> coeffectMap.

End Syntax.

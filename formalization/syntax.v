Require Import Coq.Strings.String.

(* Identifiers *)

Inductive id : Type -> Type :=
| makeId : forall T, string -> id T.

Inductive eid : Type := . (* Term id *)
Inductive tid : Type := . (* Type id *)

Definition termId := id eid.
Definition typeId := id tid.

(* Terms *)

Inductive term : Type :=
| evar : termId -> term
| eabs : termId -> type -> term -> term
| eappbv : term -> term -> term
| eappbn : term -> term -> term
| etabs : typeId -> kind -> term -> term
| etapp : term -> type -> term
| eeffect : typeId -> kind -> term -> term
| eprovide : type -> termId -> term -> term -> term

(* Types *)

with type : Type :=
| tptwithx : properType -> row -> type
| trow : row -> type
| tvar : typeId -> type
| tabs : typeId -> kind -> type -> type
| tapp : type -> type -> type

(* Proper types *)

with properType : Type :=
| ptarrow : type -> type -> properType
| ptforall : typeId -> kind -> type -> properType

(* Effect rows *)

with row : Type :=
| rempty : row
| rsingleton : type -> row
| runion : row -> row -> row

(* Kinds *)

with kind : Type :=
| ktype : kind
| krow : kind
| keffect : typeId -> termId -> type -> kind
| karrow : typeId -> kind -> kind -> kind.

(* Type contexts *)

Inductive context : Type :=
| cempty : context
| ceextend : context -> termId -> type -> context
| ctextend : context -> typeId -> kind -> context.

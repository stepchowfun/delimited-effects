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
| eunit : term
| evar : termId -> term
| eabs : termId -> type -> term -> term
| eapp : term -> term -> term
| eprovide : type -> termId -> term -> term -> term

(* Types *)

with type : Type :=
| tptwithr : properType -> row -> type
| trow : row -> type
| tvar : typeId -> type

(* Proper types *)

with properType : Type :=
| ptunit : properType
| ptarrow : type -> type -> properType

(* Effect rows *)

with row : Type :=
| rempty : row
| rsingleton : type -> row
| runion : row -> row -> row

(* Kinds *)

with kind : Type :=
| ktype : kind
| krow : kind
| keffect : typeId -> termId -> type -> kind.

(* Type contexts *)

Inductive context : Type :=
| cempty : context
| ceextend : context -> termId -> type -> context
| ctextend : context -> typeId -> kind -> context.

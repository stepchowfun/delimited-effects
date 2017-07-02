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
| csextend : context -> typeId -> kind -> context.

(* Notation "'@'" := makeId (at level 10). *)
Notation "t1 '→' t2" := (ptarrow t1 t2) (at level 38).
Notation "'λ' i '∈' t '⇒' e" := (eabs i t e) (at level 39).
Notation "'Ø'" := cempty.
Notation "c ',e' i '∈' t" := (ceextend c i t) (at level 39).
Notation "c ',s' i '∈' k" := (csextend c i k) (at level 39).

Definition eqId {X : Set} (i1 : id X) (i2 : id X) : bool :=
  match i1 with
  | makeId _ s1 => match i2 with
    | makeId _ s2 => andb (prefix s1 s2) (prefix s2 s1)
    end
  end.

Fixpoint lookupEVar (c1 : context) e :=
  match e with
  | evar i1 => match c1 with
               | cempty => None
               | ceextend c2 i2 t => if eqId i1 i2
                                     then Some t
                                     else lookupEVar c2 e
               | csextend c2 i2 k => lookupEVar c2 e
               end
  | _ => None
  end.

Fixpoint lookupTVar (c1 : context) e :=
  match e with
  | tvar i1 => match c1 with
               | cempty => None
               | ceextend c2 i2 t => lookupTVar c2 e
               | csextend c2 i2 k => if eqId i1 i2
                                     then Some k
                                     else lookupTVar c2 e
               end
  | _ => None
  end.

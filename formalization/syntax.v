Require Import Coq.Lists.List.
Require Import Coq.Strings.String.

(* Identifiers *)

Inductive id (T : Type) : Type :=
| makeId : string -> id T.

Inductive sid : Type := . (* Scheme id *)
Inductive eid : Type := . (* Term id *)

Definition schemeId := id sid.
Definition termId := id eid.

(* Terms *)

Inductive term : Type :=
| evar : termId -> term
| eabs : termId -> scheme -> term -> term
| eappbv : term -> term -> term
| eappbn : term -> term -> term
| esabs : schemeId -> kind -> term -> term
| esapp : term -> scheme -> term
| eeffect : schemeId -> kind -> term -> term
| eprovide : scheme -> termId -> term -> term -> term

(* Schemes *)

with scheme : Type :=
| stwithx : type -> row -> scheme
| srow : row -> scheme
| svar : schemeId -> scheme
| sabs : schemeId -> kind -> scheme -> scheme
| sapp : scheme -> scheme -> scheme

(* Types *)

with type : Type :=
| tarrow : scheme -> scheme -> type
| ttforall : schemeId -> kind -> scheme -> type

(* Effect rows *)

with row : Type :=
| rempty : row
| rsingleton : scheme -> row
| runion : row -> row -> row

(* Kinds *)

with kind : Type :=
| ktype : kind
| krow : kind
| keffect : schemeId -> termId -> scheme -> kind
| karrow : schemeId -> kind -> kind -> kind.

(* Type contexts *)

Inductive context : Type :=
| cempty : context
| ceextend : context -> termId -> scheme -> context
| csextend : context -> schemeId -> kind -> context.

(* Notation "'@'" := makeId (at level 10). *)
Notation "t1 '→' t2" := (tarrow t1 t2) (at level 38).
Notation "'λ' i '∈' t '⇒' e" := (eabs i t e) (at level 39).
Notation "'Ø'" := cempty.
Notation "c ',e' i '∈' s" := (ceextend c i s) (at level 39).
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
               | ceextend c2 i2 s => if eqId i1 i2 then Some s else lookupEVar c2 e
               | csextend c2 i2 k => lookupEVar c2 e
               end
  | _ => None
  end.

Fixpoint lookupSVar (c1 : context) e :=
  match e with
  | svar i1 => match c1 with
               | cempty => None
               | ceextend c2 i2 s => lookupSVar c2 e
               | csextend c2 i2 k => if eqId i1 i2 then Some k else lookupSVar c2 e
               end
  | _ => None
  end.

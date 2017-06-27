Require Import Coq.Lists.List.
Require Import Coq.Strings.String.

(* Identifiers *)

Inductive id (T : Type) : Type :=
| makeId : string -> id T.

Inductive sid : Type := . (* Type id *)
Inductive eid : Type := . (* Term id *)
Inductive rid : Type := . (* Effect row id *)
Inductive xid : Type := . (* Effect id *)

Definition schemeId := id sid.
Definition termId := id eid.

(* Terms *)

Inductive term : Type :=
| evar : termId -> term
| eabs : termId -> scheme -> term -> term
| eappbv : term -> term -> term
| eappbn : term -> term -> term
| esabs : schemeId -> term -> term
| esapp : term -> scheme -> term
| eeffect : schemeId -> kind -> term -> term
| eprovide : scheme -> termId -> term -> term -> term

(* Schemes *)

with scheme : Type :=
| stwithx : type -> effects -> scheme
| rrow : effects -> scheme
| svar : schemeId -> scheme
| sabs : schemeId -> kind -> scheme
| sapp : scheme -> scheme -> scheme

(* Types *)

with type : Type :=
| tarrow : type -> type -> type
| ttforall : schemeId -> type -> type

(* Effects *)

with effects : Type :=
| rempty : effects
| rsingleton : scheme -> effects
| runion : effects -> effects -> effects

(* Kinds *)

with kind : Type :=
| kkind : kind
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
Notation "c ',t' i '∈' s" := (ceextend c i s) (at level 39).
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

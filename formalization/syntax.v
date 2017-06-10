Require Import Coq.Lists.List.
Require Import Coq.Strings.String.

(* Identifiers *)

Inductive id (T : Type) : Type :=
| makeId : string -> id T.

Inductive tid : Type := . (* Type id *)
Inductive eid : Type := . (* Term id *)
Inductive xid : Type := . (* Effect row id *)
Inductive fid : Type := . (* Effect id *)

(* Types *)

Inductive type : Type :=
| tvar : id tid -> type
| tunit : type
| tarrow : type -> type -> type
| ttforall : id tid -> type -> type
| txforall : id xid -> type -> type.

(* Terms *)

Inductive term : Type :=
| eunit : term
| evar : id eid -> term
| eabs : id eid -> type -> term -> term
| eapp : term -> term -> term
| etabs : id tid -> term -> term
| etapp : term -> type -> term
| exabs : id xid -> term -> term
| exapp : term -> effects -> term
| eeffect : id fid -> list (id tid) -> id eid -> type -> term -> term
| eprovide : id fid -> list type -> effects -> id eid -> term -> term -> term

(* Effects *)

with effects : Type :=
| xvar : id xid -> effects
| xempty : effects
| xsingleton : id fid -> list type -> effects
| xunion : effects -> effects.

(* Type contexts *)

Inductive context : Type :=
| cempty : context
| cextend : context -> id eid -> type -> context.

(* Effect context *)

Inductive xContext : Type :=
| dempty : xContext
| dextend : xContext -> id fid -> list (id tid) -> id eid -> type -> xContext.

(* Notation "'@'" := makeId (at level 10). *)
Notation "t1 '→' t2" := (tarrow t1 t2) (at level 38).
Notation "'λ' i '∈' t '⇒' e" := (eabs i t e) (at level 39).
Notation "'Ø'" := cempty.
Notation "c ',' i '∈' t" := (cextend c i t) (at level 39).

Definition eqId (X : Set) (i1 : id X) (i2 : id X) : bool :=
  match i1 with
  | makeId _ s1 => match i2 with
    | makeId _ s2 => andb (prefix s1 s2) (prefix s2 s1)
     end
  end.

Fixpoint lookupVar (c1 : context) e :=
  match e with
  | evar i1 => match c1 with
               | cempty => None
               | cextend c2 i2 t => if eqId eid i1 i2 then Some t else lookupVar c2 e
              end
  | _ => None
  end.

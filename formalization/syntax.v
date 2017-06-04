Require Import Coq.Strings.String.

(* Identifiers *)

Inductive id : Type :=
| makeId : string -> id.

Notation "'@' s" := (makeId s) (at level 10).

Definition eqId i1 i2 : bool :=
  match i1 with
    @ s1 => match i2 with
      @ s2 => andb (prefix s1 s2) (prefix s2 s1)
    end
  end.

(* Types *)

Inductive type : Type :=
| tunit : type
| tarrow : type -> type -> type.

Notation "t1 '→' t2" := (tarrow t1 t2) (at level 38).

(* Terms *)

Inductive term : Type :=
| eunit : term
| evar : id -> term
| eabs : id -> type -> term -> term
| eapp : term -> term -> term.

Notation "'λ' i '∈' t '⇒' e" := (eabs i t e) (at level 39).

(* Contexts *)

Definition context := id -> option type.

Definition emptyContext : context := fun i => None.

Notation "'Ø'" := emptyContext.

Definition extendContext c i1 t : context :=
  fun i2 => if eqId i1 i2 then Some t else c i2.

Notation "c '⊕' i '∈' t" := (extendContext c i t) (at level 39).

Definition lookupVar (c : context) e :=
  match e with
  | evar i => c i
  | _ => None
  end.

Require Import Coq.Strings.String.

(* Identifiers *)

Inductive id : Type :=
| makeId : string -> id.

Notation "'@' s" := (makeId s) (at level 10).

Definition eqId i1 i2 : bool :=
  match i1 with
  | @ s1 => match i2 with
    | @ s2 => andb (prefix s1 s2) (prefix s2 s1)
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

Inductive context : Type :=
| cempty : context
| cextend : context -> id -> type -> context.

Notation "'Ø'" := cempty.
Notation "c ',' i '∈' t" := (cextend c i t) (at level 39).

Fixpoint lookupVar (c1 : context) e :=
  match e with
  | evar i1 => match c1 with
               | cempty => None
               | cextend c2 i2 t => if eqId i1 i2 then Some t else lookupVar c2 e
               end
  | _ => None
  end.

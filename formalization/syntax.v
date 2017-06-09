Require Import Coq.Lists.List.
Require Import Coq.Strings.String.

(* Identifiers *)

Inductive id : Type :=
| makeId : string -> id.

(* Types *)

Inductive type : Type :=
| tvar : id -> type
| tunit : type
| tarrow : type -> type -> type
| ttforall : id -> type -> type
| txforall : id -> type -> type.

(* Terms *)

Inductive term : Type :=
| eunit : term
| evar : id -> term
| eabs : id -> type -> term -> term
| eapp : term -> term -> term
| etabs : id -> term -> term
| etapp : term -> type -> term
| exabs : id -> term -> term
| exapp : term -> effects -> term
| eeffect : id -> list id -> id -> type -> term -> term
| eprovide : id -> list type -> effects -> id -> term -> term -> term

(* Effects *)

with effects : Type :=
| xvar : id -> effects
| xempty : effects
| xsingleton : id -> list type -> effects
| xunion : effects -> effects.

(* Type contexts *)

Inductive context : Type :=
| cempty : context
| cextend : context -> id -> type -> context.

(* Effect context *)
Inductive xContext : Type :=
| dempty : xContext
| dextend : xContext -> id -> list id -> id -> type -> xContext.

Notation "'@' s" := (makeId s) (at level 10).
Notation "t1 '→' t2" := (tarrow t1 t2) (at level 38).
Notation "'λ' i '∈' t '⇒' e" := (eabs i t e) (at level 39).
Notation "'Ø'" := cempty.
Notation "c ',' i '∈' t" := (cextend c i t) (at level 39).

Definition eqId i1 i2 : bool :=
  match i1 with
  | @ s1 => match i2 with
    | @ s2 => andb (prefix s1 s2) (prefix s2 s1)
    end
  end.

Fixpoint lookupVar (c1 : context) e :=
  match e with
  | evar i1 => match c1 with
               | cempty => None
               | cextend c2 i2 t => if eqId i1 i2 then Some t else lookupVar c2 e
               end
  | _ => None
  end.

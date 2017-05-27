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

(*  Typing rules *)

Reserved Notation "c '⊢' e '∈' t" (at level 40).

Inductive hasType : context -> term -> type -> Prop :=
| aunit : forall c, c ⊢ eunit ∈ tunit
| avar : forall c e t, lookupVar c e = Some t -> c ⊢ e ∈ t
| aabs : forall c i t1 t2 e,
         c ⊕ i ∈ t1 ⊢ e ∈ t2 ->
         c ⊢ λ i ∈ t1 ⇒ e ∈ t1 → t2
| aapp : forall c e1 e2 t1 t2,
         c ⊢ e1 ∈ t1 ->
         c ⊢ e2 ∈ t1 → t2 ->
         c ⊢ eapp e2 e1 ∈ t2
where "c '⊢' e '∈' t" := (hasType c e t).

(* Example proofs *)

Definition ex1 := eunit.

Theorem ex1WellTyped : Ø ⊢ ex1 ∈ tunit.
Proof. apply aunit. Qed.

Definition ex2 := λ @ "x" ∈ tunit ⇒ evar (@ "x").

Theorem ex2WellTyped : Ø ⊢ ex2 ∈ tunit → tunit.
Proof. apply aabs. apply avar. simpl. reflexivity. Qed.
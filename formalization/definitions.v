Require Import Coq.Strings.String.

(* Identifiers *)

Inductive id : Type :=
| makeId : string -> id.

Definition eqId i1 i2 : bool :=
  match i1 with
    makeId s1 => match i2 with
      makeId s2 => andb (prefix s1 s2) (prefix s2 s1)
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

Notation "'@' s" := (evar (makeId s)) (at level 10).
Notation "'λ' s '∈' t '.' e" := (eabs (makeId s) t e) (at level 39).

(* Contexts *)

Definition context := id -> option type.

Definition emptyContext : context := fun i => None.

Definition extendContext c i1 t : context :=
  fun i2 => if eqId i1 i2 then Some t else c i2.

Definition lookupVar (c : context) e :=
  match e with
  | evar i => c i
  | _ => None
  end.

Notation "'Ø'" := emptyContext.
Notation "c ',' s '∈' t" := (extendContext c (makeId s) t) (at level 39).
Notation "c '[' e ']'" := (lookupVar c e) (at level 40).

(*  Typing rules *)

Reserved Notation "c '⊢' e '∈' t" (at level 40).

Inductive hasType : context -> term -> type -> Prop :=
| aunit : forall c, c ⊢ eunit ∈ tunit
| avar : forall c e t, lookupVar c e = Some t -> c ⊢ e ∈ t
| aabs : forall c i t1 t2 e,
         (extendContext c i t1 ⊢ e ∈ t2) ->
         (c ⊢ eabs i t1 e ∈ tarrow t1 t2)
| aapp : forall c e1 e2 t1 t2,
         c ⊢ e1 ∈ t1 ->
         c ⊢ e2 ∈ tarrow t1 t2 ->
         c ⊢ eapp e2 e1 ∈ t2
where "c '⊢' e '∈' t" := (hasType c e t).

(* An example proof *)

Definition v_unit := eunit.

Theorem v_tunit : emptyContext ⊢ v_unit ∈ tunit.
Proof. apply aunit. Qed.

Definition v_id := eabs (makeId "x") tunit (evar (makeId "x")).

Print v_id.

Theorem v_tid : emptyContext ⊢ v_id ∈ tarrow tunit tunit.
Proof. apply aabs. apply avar. simpl. reflexivity. Qed.
Require Import syntax.

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

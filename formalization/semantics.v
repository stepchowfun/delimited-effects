Require Import syntax.
Require Import helpers.

(* Values *)

Inductive value : term -> Prop :=
| valUnit :
  value eunit
| valAbs :
  forall e x t,
  value (eabs x t e).

(* Operational semantics *)

Inductive step : term -> term -> Prop :=
| stepAppByV :
  forall e x t v,
  value v ->
  step (eappbv (eabs x t e) v) (substTermInTerm e x v)
| stepAppByN :
  forall e1 e2 x t,
  step (eappbn (eabs x t e1) e2) (substTermInTerm e1 x e2)
| stepEffect :
  forall e1 e2 a k,
  step e1 e2 ->
  step (eeffect a k e1) (eeffect a k e2)
| stepEffectVal :
  forall a k v,
  value v ->
  step (eeffect a k v) v
| stepProvide :
  forall e1 e2 e3 x t,
  step (substTermInTerm e1 x e3) e2 ->
  step (eprovide t x e3 e1) (eprovide t x e3 e2)
| stepProvideVal :
  forall e x t v,
  value v ->
  step (eprovide t x e v) (substTermInTerm v x e).

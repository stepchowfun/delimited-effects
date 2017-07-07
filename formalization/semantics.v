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

Inductive step : dcontext -> term -> term -> Prop :=
| stepAppByV :
  forall e x t v d,
  value v ->
  step d (eappbv (eabs x t e) v) (substTermInTerm e x v)
| stepAppByN :
  forall e1 e2 x t d,
  step d (eappbn (eabs x t e1) e2) (substTermInTerm e1 x e2)
| stepEffect :
  forall e a k d,
  step d (eeffect a k e) e
| stepProvide :
  forall e1 e2 x t d,
  step (dcextend d x e1) (eprovide t x e1 e2) e2.

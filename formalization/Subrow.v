(*****************************)
(*****************************)
(****                     ****)
(****   Row subsumption   ****)
(****                     ****)
(*****************************)
(*****************************)

Require Import Main.Syntax.

Inductive subrow : row -> row -> Prop :=
| rsEmpty :
  forall r, subrow rEmpty r
| rsSingletonRefl :
  forall a,
  subrow (rSingleton a) (rSingleton a)
| rsSingletonLeft :
  forall a r1 r2,
  subrow (rSingleton a) r1 ->
  subrow (rSingleton a) (rUnion r1 r2)
| rsSingletonRight :
  forall a r1 r2,
  subrow (rSingleton a) r2 ->
  subrow (rSingleton a) (rUnion r1 r2)
| rsUnion :
  forall r1 r2 r3,
  subrow r1 r3 ->
  subrow r2 r3 ->
  subrow (rUnion r1 r2) r3.

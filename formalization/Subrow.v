(*****************************)
(*****************************)
(****                     ****)
(****   Row subsumption   ****)
(****                     ****)
(*****************************)
(*****************************)

Require Import Main.Name.
Require Import Main.Syntax.
Require Import Main.Tactics.

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

Hint Constructors subrow.

Inductive rowContains : row -> name -> Prop :=
| rcSingleton :
  forall a,
  rowContains (rSingleton a) a
| rcUnionLeft :
  forall r1 r2 a,
  rowContains r1 a ->
  rowContains (rUnion r1 r2) a
| rcUnionRight :
  forall r1 r2 a,
  rowContains r2 a ->
  rowContains (rUnion r1 r2) a.

Hint Constructors rowContains.

Theorem subrowCorrect :
  forall r1 r2,
  (forall a, rowContains r1 a -> rowContains r2 a) <-> subrow r1 r2.
Proof.
  split; intros.
  - induction r1; magic.
    specialize (H n); feed H; magic.
    induction r2; inversion H; magic.
  - induction H; inversion H0; magic.
Qed.

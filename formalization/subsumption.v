Require Import syntax.
Require Import judgments.

Inductive rowContains : row -> scheme -> Prop :=
| rcSingleton :
    forall s,
    rowContains (rsingleton s) s
| rcUnionLeft :
    forall r1 r2 s,
    rowContains r1 s ->
    rowContains (runion r1 r2) s
| rcUnionRight :
    forall r1 r2 s,
    rowContains r2 s ->
    rowContains (runion r1 r2) s.

Lemma containmentImpliesSubsumption :
  forall r1 r2, (
    forall s1,
    rowContains r1 s1 ->
    exists s2,
    (schemeEq s1 s2 /\ rowContains r2 s2)
  ) ->
  subsumes r1 r2.
Admitted.

Lemma subsumptionImpliesContainment :
  forall r1 r2,
  subsumes r1 r2 -> (
    forall s1,
    rowContains r1 s1 ->
    exists s2,
    (schemeEq s1 s2 /\ rowContains r2 s2)
  ).
Admitted.

Theorem subsumptionCorrect :
  forall r1 r2, (
    forall s1,
    rowContains r1 s1 ->
    exists s2,
    (schemeEq s1 s2 /\ rowContains r2 s2)
  ) <->
  subsumes r1 r2.
Proof.
  split.
  - apply containmentImpliesSubsumption.
  - apply subsumptionImpliesContainment.
Qed.

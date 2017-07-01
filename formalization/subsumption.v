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

Definition containment r1 r2 :=
  forall s1,
  rowContains r1 s1 ->
  exists s2,
  (subtype s1 s2 /\ rowContains r2 s2).

Lemma containmentImpliesSubsumption :
  forall r1 r2,
  containment r1 r2 ->
  subsumes r1 r2.
Admitted.

Lemma subsumptionImpliesContainment :
  forall r1 r2,
  subsumes r1 r2 ->
  containment r1 r2.
Proof.
  intros r1 r2 H.
  unfold containment.
  induction H.
  (* rRefl *)
  - intros.
    exists s1.
    split.
    + apply stRefl.
    + auto.
  (* rTrans *)
  - intros.
    destruct IHsubsumes1 with (s1 := s1).
    + auto.
    + elim H2. intros.
      destruct IHsubsumes2 with (s1 := x).
      * auto.
      * {
        elim H5. intros.
        exists x0.
        split.
        - apply stTrans with (s2 := x); auto.
        - auto.
      }
  (* rEmpty *)
  - intros.
    exists s1.
    inversion H.
  (* rSingleton *)
  - intros.
    exists s2.
    intros.
    inversion H0.
    assert (subtype s0 s2).
    + apply stTrans with (s2 := s1).
      * rewrite H1.
        apply stRefl.
      * auto.
    + split.
      * auto.
      * apply rcSingleton.
  (* rUnion *)
  - intros.
    inversion H1.
    + destruct IHsubsumes1 with (s1 := s1).
      * auto.
      * exists x.
        auto.
    + destruct IHsubsumes2 with (s1 := s1).
      * auto.
      * exists x.
        auto.
  (* rWeaken *)
  - intros.
    exists s1.
    split.
    + apply stRefl.
    + inversion H.
      * apply rcUnionLeft.
        apply rcSingleton.
      * rewrite H1.
        apply rcUnionLeft.
        auto.
      * rewrite H1.
        apply rcUnionLeft.
        auto.
  (* rExchange *)
  - intros.
    exists s1.
    split.
    + apply stRefl.
    + inversion H.
      * apply rcUnionRight.
        auto.
      * apply rcUnionLeft.
        auto.
  (* rAssoc *)
  - intros.
    destruct IHsubsumes with (s1 := s1).
    + apply rcUnionLeft.
      auto.
    + elim H1. intros.
      exists x.
      split.
      * auto.
      * apply rcUnionRight.
        auto.
Qed.

Theorem subsumptionCorrect :
  forall r1 r2,
  containment r1 r2 <->
  subsumes r1 r2.
Proof.
  split.
  - apply containmentImpliesSubsumption.
  - apply subsumptionImpliesContainment.
Qed.

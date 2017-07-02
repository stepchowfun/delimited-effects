Require Import syntax.
Require Import judgments.

Theorem rRefl : forall r, subsumes r r.
Proof.
  intros.
  induction r.
  - apply rEmpty.
  - apply rSingleton.
    apply stRefl.
  - apply rUnion.
    + apply rWeaken.
    + apply rTrans with (r2 := runion r2 r1).
      * apply rWeaken.
      * apply rExchange.
Qed.

Theorem rAssoc :
  forall r1 r2 r3,
  subsumes (runion (runion r1 r2) r3) (runion r1 (runion r2 r3)).
Proof.
  intros.
  apply rUnion.
  - apply rUnion.
    + apply rWeaken.
    + apply rTrans with (r2 := runion (runion r2 r3) r1).
      * apply rTrans with (r2 := runion r2 r3); apply rWeaken.
      * apply rExchange.
  - apply rTrans with (r2 := runion (runion r2 r3) r1).
    + apply rTrans with (r2 := runion r2 r3).
      * {
        apply rTrans with (r2 := runion r3 r2).
        - apply rWeaken.
        - apply rExchange.
      }
      * apply rWeaken.
    + apply rExchange.
Qed.

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

Lemma subsumptionImpliesContainment :
  forall r1 r2,
  subsumes r1 r2 ->
  containment r1 r2.
Proof.
  intros r1 r2 H.
  unfold containment.
  induction H.
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
Qed.

Lemma containmentImpliesSubsumption :
  forall r1 r2,
  containment r1 r2 ->
  subsumes r1 r2.
Proof.
  intros.
  unfold containment in H.
  induction r1.
  (* rempty *)
  - apply rEmpty.
  (* rsingleton *)
  - induction r2.
    (* rempty *)
    + set (H1 := H s).
      set (H2 := H1 (rcSingleton s)).
      elim H2. intros.
      elim H0. intros.
      inversion H4.
    (* rsingleton *)
    + set (H1 := H s).
      set (H2 := H1 (rcSingleton s)).
      elim H2. intros.
      elim H0. intros.
      inversion H4.
      apply rSingleton. auto.
    (* runion *)
    + set (H1 := H s).
      set (H2 := H1 (rcSingleton s)).
      elim H2. intros.
      elim H0. intros.
      inversion H4.
      * {
        assert (
          forall s1 : scheme,
          rowContains (rsingleton s) s1 ->
          exists s2 : scheme,
          subtype s1 s2 /\ rowContains r2_1 s2
        ).
        - intros.
          exists x.
          inversion H9.
          rewrite H10 in H3. auto.
        - apply IHr2_1 in H9.
          assert (subsumes r2_1 (runion r2_1 r2_2)).
          + apply rWeaken.
          + apply rTrans with (r2 := r2_1); auto.
      }
      * {
        assert (
          forall s1 : scheme,
          rowContains (rsingleton s) s1 ->
          exists s2 : scheme,
          subtype s1 s2 /\ rowContains r2_2 s2
        ).
        - intros.
          exists x.
          inversion H9.
          rewrite H10 in H3. auto.
        - apply IHr2_2 in H9.
          assert (subsumes r2_2 (runion r2_1 r2_2)).
          + apply rTrans with (r2 := runion r2_2 r2_1).
            * apply rWeaken.
            * apply rExchange.
          + apply rTrans with (r2 := r2_2); auto.
      }
  (* runion *)
  - assert (
      forall s1 : scheme,
      rowContains r1_1 s1 ->
      exists s2 : scheme,
      subtype s1 s2 /\ rowContains r2 s2
    ).
    + intros.
      set (H1 := H s1).
      assert (rowContains (runion r1_1 r1_2) s1).
      * apply rcUnionLeft. auto.
      * set (H3 := H1 H2).
        auto.
    + assert (
        forall s1 : scheme,
        rowContains r1_2 s1 ->
        exists s2 : scheme,
        subtype s1 s2 /\ rowContains r2 s2
      ).
      * {
        intros.
        set (H2 := H s1).
        assert (rowContains (runion r1_1 r1_2) s1).
        - apply rcUnionRight. auto.
        - set (H4 := H2 H3).
          auto.
      }
      * apply rUnion; auto.
Qed.

Theorem subsumptionCorrect :
  forall r1 r2,
  subsumes r1 r2 <->
  containment r1 r2.
Proof.
  split.
  - apply subsumptionImpliesContainment.
  - apply containmentImpliesSubsumption.
Qed.

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

Inductive rowContains : row -> type -> Prop :=
| rcSingleton :
    forall t,
    rowContains (rsingleton t) t
| rcUnionLeft :
    forall r1 r2 t,
    rowContains r1 t ->
    rowContains (runion r1 r2) t
| rcUnionRight :
    forall r1 r2 t,
    rowContains r2 t ->
    rowContains (runion r1 r2) t.

Definition containment r1 r2 :=
  forall t1,
  rowContains r1 t1 ->
  exists t2,
  (subtype t1 t2 /\ rowContains r2 t2).

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
    destruct IHsubsumes1 with (t1 := t1).
    + auto.
    + elim H2. intros.
      destruct IHsubsumes2 with (t1 := x).
      * auto.
      * {
        elim H5. intros.
        exists x0.
        split.
        - apply stTrans with (t2 := x); auto.
        - auto.
      }
  (* rEmpty *)
  - intros.
    exists t1.
    inversion H.
  (* rSingleton *)
  - intros.
    exists t2.
    intros.
    inversion H0.
    assert (subtype t0 t2).
    + apply stTrans with (t2 := t1).
      * rewrite H1.
        apply stRefl.
      * auto.
    + split.
      * auto.
      * apply rcSingleton.
  (* rUnion *)
  - intros.
    inversion H1.
    + destruct IHsubsumes1 with (t1 := t1).
      * auto.
      * exists x.
        auto.
    + destruct IHsubsumes2 with (t1 := t1).
      * auto.
      * exists x.
        auto.
  (* rWeaken *)
  - intros.
    exists t1.
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
    exists t1.
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
    + set (H1 := H t).
      set (H2 := H1 (rcSingleton t)).
      elim H2. intros.
      elim H0. intros.
      inversion H4.
    (* rsingleton *)
    + set (H1 := H t).
      set (H2 := H1 (rcSingleton t)).
      elim H2. intros.
      elim H0. intros.
      inversion H4.
      apply rSingleton. auto.
    (* runion *)
    + set (H1 := H t).
      set (H2 := H1 (rcSingleton t)).
      elim H2. intros.
      elim H0. intros.
      inversion H4.
      * {
        assert (
          forall t1 : type,
          rowContains (rsingleton t) t1 ->
          exists t2 : type,
          subtype t1 t2 /\ rowContains r2_1 t2
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
          forall t1 : type,
          rowContains (rsingleton t) t1 ->
          exists t2 : type,
          subtype t1 t2 /\ rowContains r2_2 t2
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
      forall t1 : type,
      rowContains r1_1 t1 ->
      exists t2 : type,
      subtype t1 t2 /\ rowContains r2 t2
    ).
    + intros.
      set (H1 := H t1).
      assert (rowContains (runion r1_1 r1_2) t1).
      * apply rcUnionLeft. auto.
      * set (H3 := H1 H2).
        auto.
    + assert (
        forall t1 : type,
        rowContains r1_2 t1 ->
        exists t2 : type,
        subtype t1 t2 /\ rowContains r2 t2
      ).
      * {
        intros.
        set (H2 := H t1).
        assert (rowContains (runion r1_1 r1_2) t1).
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

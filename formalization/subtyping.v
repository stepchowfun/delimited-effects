Require Import syntax.
Require Import judgments.

Theorem stAssoc :
  forall r1 r2 r3,
  subtype (trow (runion (runion r1 r2) r3)) (trow (runion r1 (runion r2 r3))).
Proof.
  intros r1 r2 r3.
  apply stUnion.
  - apply stUnion.
    + apply stWeaken.
    + apply stTrans with (t2 := trow (runion (runion r2 r3) r1)).
      * apply stTrans with (t2 := trow (runion r2 r3)); apply stWeaken.
      * apply stExchange.
  - apply stTrans with (t2 := trow (runion (runion r2 r3) r1)).
    + apply stTrans with (t2 := trow (runion r2 r3)).
      * {
        apply stTrans with (t2 := trow (runion r3 r2)).
        - apply stWeaken.
        - apply stExchange.
      }
      * apply stWeaken.
    + apply stExchange.
Qed.

Definition subset r1 r2 :=
  forall t1,
  rowContains r1 t1 ->
  exists t2,
  (subtype t1 t2 /\ rowContains r2 t2).

Lemma subsetTrans :
  forall r1 r2 r3,
  subset r1 r2 ->
  subset r2 r3 ->
  subset r1 r3.
Proof.
  intros r1 r2 r3 H1 H2.
  unfold subset in *.
  intros t1 H3.
  set (H4 := H1 t1 H3).
  destruct H4 as [t2 H4].
  destruct H4 as [H4 H5].
  set (H6 := H2 t2 H5).
  destruct H6 as [t3 H6].
  destruct H6 as [H6 H7].
  exists t3.
  split.
  - apply stTrans with (t2 := t2); auto.
  - auto.
Qed.

Lemma rownessClosedUnderSubtype :
  forall t1 t2,
  subtype t1 t2 ->
  forall r1,
  t1 = trow r1 ->
  exists r2,
  t2 = trow r2.
Proof.
  intros t1 t2 H1.
  induction H1 as [
    t1                                  | (* stRefl *)
    t1 t2 t3 H1 H2 H3 H4                | (* stTrans *)
    t1 t2 t3 t4 r1 r2 H1 H2 H3 H4 H5 H6 | (* stArrow *)
    t1 t2 a1 r1 r2 k1 H1 H2 H3 H4       | (* stForAll *)
    r1                                  | (* stEmpty *)
    t1 t2 H1 H2                         | (* stSingleton *)
    t1 r1 r2 H1 H2 H3 H4                | (* stUnion *)
    r1 r2                               | (* stWeaken *)
    r1 r2                               | (* stExchange *)
    t1 t2 a1 k1 H1 H2                   | (* stTypeAbs *)
    t1 t2 t3 H1 H2                        (* stTypeApp *)
  ].
  (* stRefl *)
  - intros r1 H1.
    exists r1.
    auto.
  (* stTrans *)
  - intros r1 H5.
    set (H6 := H2 r1 H5).
    destruct H6 as [r2 H6].
    set (H7 := H4 r2 H6).
    auto.
  (* stArrow *)
  - intros r3 H7.
    inversion H7.
  (* stForAll *)
  - intros r3 H5.
    inversion H5.
  (* stEmpty *)
  - intros r2.
    exists r1.
    reflexivity.
  (* stSingleton *)
  - intros r1 H3.
    exists (rsingleton t2).
    reflexivity.
  (* stUnion *)
  - intros r3 H5.
    set (H6 := H2 r1 eq_refl).
    destruct H6 as [r4 H6].
    exists r4.
    auto.
  (* stWeaken *)
  - intros r3 H1.
    exists (runion r1 r2).
    reflexivity.
  (* stExchange *)
  - intros r3 H1.
    exists (runion r2 r1).
    reflexivity.
  (* stTypeAbs *)
  - intros r1 H3.
    inversion H3.
  (* stTypeApp *)
  - intros r1 H3.
    inversion H3.
Qed.

Lemma subtypeImpliesSubsetAux :
  forall t1 t2,
  subtype t1 t2 ->
  match t1 with
  | trow r1 => match t2 with
               | trow r2 => subset r1 r2
               | _ => True
               end
  | _ => True
  end.
Proof.
  intros t1 t2 H1.
  induction H1 as [
    t1                                  | (* stRefl *)
    t1 t2 t3 H1 H2 H3 H4                | (* stTrans *)
    t1 t2 t3 t4 r1 r2 H1 H2 H3 H4 H5 H6 | (* stArrow *)
    t1 t2 a1 r1 r2 k1 H1 H2 H3 H4       | (* stForAll *)
    r1                                  | (* stEmpty *)
    t1 t2 H1 H2                         | (* stSingleton *)
    t1 r1 r2 H1 H2 H3 H4                | (* stUnion *)
    r1 r2                               | (* stWeaken *)
    r1 r2                               | (* stExchange *)
    t1 t2 a1 k1 H1 H2                   | (* stTypeAbs *)
    t1 t2 t3 H1 H2                        (* stTypeApp *)
  ]; auto.
  (* stRefl *)
  - destruct t1 as [
      pt1 r1     | (* tptwithx *)
      r1         | (* trow *)
      tid1       | (* tvar *)
      tid1 k1 t1 | (* tabs *)
      t1 t2        (* tapp *)
    ]; auto.
    unfold subset.
    intros t1 H1.
    exists t1.
    split.
    + apply stRefl.
    + auto.
  (* stTrans *)
  - induction t1 as [
      pt1 r1     | (* tptwithx *)
      r1         | (* trow *)
      tid1       | (* tvar *)
      tid1 k1 t1 | (* tabs *)
      t1 t4        (* tapp *)
    ]; auto.
    assert (exists r2, t2 = trow r2) as H5.
    + apply rownessClosedUnderSubtype with (t1 := trow r1) (t2 := t2) (r1 := r1); auto.
    + destruct H5 as [r2 H5].
      rewrite H5 in H2.
      rewrite H5 in H4.
      induction t3; auto.
      apply subsetTrans with (r2 := r2); auto.
  (* stEmpty *)
  - unfold subset.
    intros t1 H1.
    exists t1.
    split.
    + apply stRefl.
    + inversion H1.
  (* stSingleton *)
  - unfold subset.
    intros t3 H3.
    exists t2.
    inversion H3 as [
      t4 H4 H5 | (* rcSingleton *)
               | (* rcUnionLeft *)
                 (* rcUnionRight *)
    ].
    split.
    + rewrite H5 in H1.
      auto.
    + apply rcSingleton.
  (* stUnion *)
  - destruct t1 as [
      pt1 r3     | (* tptwithx *)
      r3         | (* trow *)
      tid1       | (* tvar *)
      tid1 k1 t1 | (* tabs *)
      t1 t4        (* tapp *)
    ]; auto.
    unfold subset.
    intros t1 H5.
    unfold subset in H2.
    unfold subset in H4.
    inversion H5 as [
      t2 H6 H7          | (* rcSingleton *)
      r4 r5 t2 H6 H7 H8 | (* rcUnionLeft *)
      r4 r5 t2 H6 H7 H8   (* rcUnionRight *)
    ].
    + set (H9 := H2 t1 H6).
      auto.
    + set (H9 := H4 t1 H6).
      auto.
  (* stWeaken *)
  - unfold subset.
    intros t1 H1.
    exists t1.
    split.
    + apply stRefl.
    + apply rcUnionLeft.
      auto.
  (* stExchange *)
  - unfold subset.
    intros t1 H1.
    exists t1.
    split.
    + apply stRefl.
    + inversion H1.
      * apply rcUnionRight.
        auto.
      * apply rcUnionLeft.
        auto.
Qed.

Lemma subtypeImpliesSubset :
  forall r1 r2,
  subtype (trow r1) (trow r2) ->
  subset r1 r2.
Proof.
  intros r1 r2.
  apply (subtypeImpliesSubsetAux (trow r1) (trow r2)).
Qed.

Lemma subsetImpliesSubtype :
  forall r1 r2,
  subset r1 r2 ->
  subtype (trow r1) (trow r2).
Proof.
  intros r1 r2 H1.
  unfold subset in H1.
  induction r1 as [
                | (* rempty *)
    t1          | (* rsingleton *)
    r1 H2 r3 H3   (* runion *)
  ].
  (* rempty *)
  - apply stEmpty.
  (* rsingleton *)
  - induction r2 as [
                  | (* rempty *)
      t2          | (* rsingleton *)
      r1 H2 r3 H3   (* runion *)
    ].
    (* rempty *)
    + set (H2 := H1 t1).
      set (H3 := H2 (rcSingleton t1)).
      destruct H3 as [t2 H3].
      destruct H3 as [t3 H3].
      inversion H3.
    (* rsingleton *)
    + set (H2 := H1 t1).
      set (H3 := H2 (rcSingleton t1)).
      destruct H3 as [t3 H3].
      destruct H3 as [t4 H3].
      inversion H3.
      apply stSingleton. auto.
    (* runion *)
    + set (H4 := H1 t1).
      set (H5 := H4 (rcSingleton t1)).
      destruct H5 as [t2 H5].
      destruct H5 as [H5 H6].
      inversion H6 as [
        t4 H7 H8          | (* rcSingleton *)
        r2 r4 t4 H7 H8 H9 | (* rcUnionLeft *)
        r2 r4 t4 H7 H8 H9   (* rcUnionRight *)
      ].
      * {
        assert (
          forall t2 : type,
          rowContains (rsingleton t1) t2 ->
          exists t3 : type,
          subtype t2 t3 /\ rowContains r1 t3
        ) as H10.
        - intros t5 H10.
          exists t2.
          inversion H10 as [
            t6 H11 H12           | (* rcSingleton *)
            r5 r6 t6 H11 H12 H13 | (* rcUnionLeft *)
            r5 r6 t6 H11 H12 H13   (* rcUnionRight *)
          ].
          rewrite H12 in H5. auto.
        - apply H2 in H10.
          assert (subtype (trow r1) (trow (runion r1 r3))).
          + apply stWeaken.
          + apply stTrans with (t2 := trow r1); auto.
      }
      * {
        assert (
          forall t2 : type,
          rowContains (rsingleton t1) t2 ->
          exists t3 : type,
          subtype t2 t3 /\ rowContains r3 t3
        ).
        - intros t5 H10.
          exists t2.
          inversion H10 as [
            t6 H11 H12           | (* rcSingleton *)
            r5 r6 t6 H11 H12 H13 | (* rcUnionLeft *)
            r5 r6 t6 H11 H12 H13   (* rcUnionRight *)
          ].
          rewrite H12 in H5. auto.
        - apply H3 in H0.
          assert (subtype (trow r3) (trow (runion r1 r3))).
          + apply stTrans with (t2 := trow (runion r3 r1)).
            * apply stWeaken.
            * apply stExchange.
          + apply stTrans with (t2 := trow r3); auto.
      }
  (* runion *)
  - assert (
      forall t1 : type,
      rowContains r1 t1 ->
      exists t2 : type,
      subtype t1 t2 /\ rowContains r2 t2
    ) as H4.
    + intros t1 H5.
      set (H6 := H1 t1).
      assert (rowContains (runion r1 r3) t1) as H7.
      * apply rcUnionLeft. auto.
      * set (H8 := H6 H7).
        auto.
    + assert (
        forall t1 : type,
        rowContains r3 t1 ->
        exists t2 : type,
        subtype t1 t2 /\ rowContains r2 t2
      ) as H5.
      * {
        intros t1 H5.
        set (H6 := H1 t1).
        assert (rowContains (runion r1 r3) t1) as H7.
        - apply rcUnionRight. auto.
        - set (H8 := H6 H7).
          auto.
      }
      * apply stUnion; auto.
Qed.

Theorem subtypeCorrect :
  forall r1 r2,
  subtype (trow r1) (trow r2) <->
  subset r1 r2.
Proof.
  split.
  - apply subtypeImpliesSubset.
  - apply subsetImpliesSubtype.
Qed.

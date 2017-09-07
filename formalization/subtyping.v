Require Import syntax.
Require Import judgments.

Theorem stExchange :
  forall r1 r2,
  subtype (trow (runion r1 r2)) (trow (runion r2 r1)).
Proof.
  intros r1 r2.
  apply stUnion.
  - apply stWeakenRight.
  - apply stWeakenLeft.
Qed.

Theorem stAssoc :
  forall r1 r2 r3,
  subtype (trow (runion (runion r1 r2) r3)) (trow (runion r1 (runion r2 r3))).
Proof.
  intros r1 r2 r3.
  apply stUnion.
  - apply stUnion.
    + apply stWeakenLeft.
    + apply stTrans with (t2 := trow (runion r2 r3)).
      * apply stWeakenLeft.
      * apply stWeakenRight.
  - apply stTrans with (t2 := trow (runion r2 r3)); apply stWeakenRight.
Qed.

Definition subset r1 r2 := forall t, rowContains r1 t -> rowContains r2 t.

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
  set (H5 := H2 t1 H4).
  auto.
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
    r1                                  | (* stEmpty *)
    t1 r1 r2 H1 H2 H3 H4                | (* stUnion *)
    r1 r2                               | (* stWeakenLeft *)
    r1 r2                                 (* stExchange *)
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
  (* stEmpty *)
  - intros r2.
    exists r1.
    reflexivity.
  (* stUnion *)
  - intros r3 H5.
    set (H6 := H2 r1 eq_refl).
    destruct H6 as [r4 H6].
    exists r4.
    auto.
  (* stWeakenLeft *)
  - intros r3 H1.
    exists (runion r1 r2).
    reflexivity.
  (* stWeakenRight *)
  - intros r3 H1.
    exists (runion r1 r2).
    reflexivity.
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
    r1                                  | (* stEmpty *)
    t1 r1 r2 H1 H2 H3 H4                | (* stUnion *)
    r1 r2                               | (* stWeakenLeft *)
    r1 r2                                 (* stExchange *)
  ]; auto.
  (* stRefl *)
  - destruct t1 as [
      pt1 r1     | (* tptwithx *)
      r1         | (* trow *)
      tid1         (* tvar *)
    ]; auto.
    unfold subset.
    intros t1 H1.
    auto.
  (* stTrans *)
  - induction t1 as [
      pt1 r1     | (* tptwithx *)
      r1         | (* trow *)
      tid1         (* tvar *)
    ]; auto.
    assert (exists r2, t2 = trow r2) as H5.
    + apply rownessClosedUnderSubtype
      with (t1 := trow r1) (t2 := t2) (r1 := r1); auto.
    + destruct H5 as [r2 H5].
      rewrite H5 in H2.
      rewrite H5 in H4.
      induction t3; auto.
      apply subsetTrans with (r2 := r2); auto.
  (* stEmpty *)
  - unfold subset.
    intros t1 H1.
    inversion H1.
  (* stUnion *)
  - destruct t1 as [
      pt1 r3     | (* tptwithx *)
      r3         | (* trow *)
      tid1         (* tvar *)
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
  (* stWeakenLeft *)
  - unfold subset.
    intros t1 H1.
    apply rcUnionLeft.
    auto.
  (* stWeakenRight *)
  - unfold subset.
    intros t1 H1.
    apply rcUnionRight.
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
      r1 H2 r2 H3   (* runion *)
    ].
    (* rempty *)
    + set (H2 := H1 t1 (rcSingleton t1)).
      inversion H2.
    (* rsingleton *)
    + set (H2 := H1 t1 (rcSingleton t1)).
      inversion H2.
      apply stRefl.
    (* runion *)
    + set (H4 := H1 t1 (rcSingleton t1)).
      inversion H4 as [
        t2 H5 H6          | (* rcSingleton *)
        r3 r4 t2 H5 H6 H7 | (* rcUnionLeft *)
        r3 r4 t2 H5 H6 H7   (* rcUnionRight *)
      ].
      * {
        apply stTrans with (t2 := trow r1).
        - apply H2.
          intros t3 H8.
          inversion H8 as [
            t4 H9 H10          | (* rcSingleton *)
            r5 r6 t4 H9 H10 H11 | (* rcUnionLeft *)
            r5 r6 t4 H9 H10 H11   (* rcUnionRight *)
          ].
          rewrite <- H10.
          auto.
        - apply stWeakenLeft.
      }
      * {
        apply stTrans with (t2 := trow r2).
        - apply H3.
          intros t3 H8.
          inversion H8 as [
            t4 H9 H10          | (* rcSingleton *)
            r5 r6 t4 H9 H10 H11 | (* rcUnionLeft *)
            r5 r6 t4 H9 H10 H11   (* rcUnionRight *)
          ].
          rewrite <- H10.
          auto.
        - apply stWeakenRight.
      }
  (* runion *)
  - apply stUnion.
    + apply H2.
      intros t1 H4.
      apply H1.
      apply rcUnionLeft.
      auto.
    + apply H3.
      intros t1 H4.
      apply H1.
      apply rcUnionRight.
      auto.
Qed.

Theorem effectRowSubtyping :
  forall r1 r2,
  subtype (trow r1) (trow r2) <->
  subset r1 r2.
Proof.
  split.
  - apply subtypeImpliesSubset.
  - apply subsetImpliesSubtype.
Qed.

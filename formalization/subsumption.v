Require Import syntax.
Require Import judgments.

Theorem stAssoc :
  forall r1 r2 r3,
  subtype (trow (runion (runion r1 r2) r3)) (trow (runion r1 (runion r2 r3))).
Proof.
  intros.
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
  intros.
  unfold subset in *.
  intros.
  set (H2 := H t1 H1).
  destruct H2.
  destruct H2.
  set (H4 := H0 x H3).
  destruct H4.
  destruct H4.
  exists x0.
  split.
  - apply stTrans with (t2 := x); auto.
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
  intros t1 t2 H.
  induction H.
  (* stRefl *)
  - intros.
    exists r1.
    auto.
  (* stTrans *)
  - intros.
    set (H2 := IHsubtype1 r1 H1).
    destruct H2.
    set (H3 := IHsubtype2 x H2).
    auto.
  (* stArrow *)
  - intros.
    inversion H2.
  (* stForAll *)
  - intros.
    inversion H1.
  (* stEmpty *)
  - intros.
    exists r.
    reflexivity.
  (* stSingleton *)
  - intros.
    exists (rsingleton t2).
    reflexivity.
  (* stUnion *)
  - intros.
    set (H2 := IHsubtype1 r1 eq_refl).
    destruct H2.
    exists x.
    auto.
  (* stWeaken *)
  - intros.
    exists (runion r1 r2).
    reflexivity.
  (* stExchange *)
  - intros.
    exists (runion r2 r1).
    reflexivity.
  (* stTypeAbs *)
  - intros.
    inversion H0.
  (* stTypeApp *)
  - intros.
    inversion H0.
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
  intros.
  induction H; auto.
  (* stRefl *)
  - destruct t; auto.
    unfold subset.
    intros.
    exists t1.
    split.
    + apply stRefl.
    + auto.
  (* stTrans *)
  - induction t1; auto.
    assert (exists r2, t2 = trow r2).
    + apply rownessClosedUnderSubtype with (t1 := trow r) (t2 := t2) (r1 := r); auto.
    + destruct H1.
      rewrite H1 in IHsubtype1.
      rewrite H1 in IHsubtype2.
      induction t3; auto.
      apply subsetTrans with (r2 := x); auto.
  (* stEmpty *)
  - unfold subset.
    intros.
    exists t1.
    split.
    + apply stRefl.
    + inversion H.
  (* stSingleton *)
  - unfold subset.
    intros.
    exists t2.
    inversion H0.
    split.
    + rewrite H1 in H.
      auto.
    + apply rcSingleton.
  (* stUnion *)
  - destruct t; auto.
    unfold subset.
    intros.
    unfold subset in IHsubtype1.
    unfold subset in IHsubtype2.
    inversion H1.
    + set (H6 := IHsubtype1 t1 H5).
      auto.
    + set (H6 := IHsubtype2 t1 H5).
      auto.
  (* stWeaken *)
  - unfold subset.
    intros.
    exists t1.
    split.
    + apply stRefl.
    + apply rcUnionLeft.
      auto.
  (* stExchange *)
  - unfold subset.
    intros.
    exists t1.
    split.
    + apply stRefl.
    + inversion H.
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
  intros.
  apply (subtypeImpliesSubsetAux (trow r1) (trow r2)).
  auto.
Qed.

Lemma subsetImpliesSubtype :
  forall r1 r2,
  subset r1 r2 ->
  subtype (trow r1) (trow r2).
Proof.
  intros.
  unfold subset in H.
  induction r1.
  (* rempty *)
  - apply stEmpty.
  (* rsingleton *)
  - induction r2.
    (* rempty *)
    + set (H1 := H t).
      set (H2 := H1 (rcSingleton t)).
      destruct H2.
      destruct H0.
      inversion H2.
    (* rsingleton *)
    + set (H1 := H t).
      set (H2 := H1 (rcSingleton t)).
      destruct H2.
      destruct H0.
      inversion H2.
      apply stSingleton. auto.
    (* runion *)
    + set (H1 := H t).
      set (H2 := H1 (rcSingleton t)).
      destruct H2.
      destruct H0.
      inversion H2.
      * {
        assert (
          forall t1 : type,
          rowContains (rsingleton t) t1 ->
          exists t2 : type,
          subtype t1 t2 /\ rowContains r2_1 t2
        ).
        - intros.
          exists x.
          inversion H7.
          rewrite H8 in H0. auto.
        - apply IHr2_1 in H7.
          assert (subtype (trow r2_1) (trow (runion r2_1 r2_2))).
          + apply stWeaken.
          + apply stTrans with (t2 := trow r2_1); auto.
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
          inversion H7.
          rewrite H8 in H0. auto.
        - apply IHr2_2 in H7.
          assert (subtype (trow r2_2) (trow (runion r2_1 r2_2))).
          + apply stTrans with (t2 := trow (runion r2_2 r2_1)).
            * apply stWeaken.
            * apply stExchange.
          + apply stTrans with (t2 := trow r2_2); auto.
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

Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import syntax.

Lemma eqIdDec : forall {X} (id1 id2 : id X), {id1 = id2} + {id1 <> id2}.
Proof.
  intros.
  destruct id1 as [t s1].
  destruct id2 as [t s2].
  set (P := string_dec s1 s2).
  destruct P as [H1 | H1].
  - assert (makeId t s1 = makeId t s2).
    + rewrite H1.
      reflexivity.
    + apply left.
      auto.
  - assert (makeId t s1 <> makeId t s2).
    + unfold not.
      intros H2.
      inversion H2.
      contradiction.
    + apply right.
      auto.
Qed.

Definition expandArgs (params : list (typeId * kind)) (k1 : kind) :=
  fold_right (
    fun (param : typeId * kind) k2 =>
      match param with
      | (a, k3) => karrow a k3 k2
      end
  ) k1 params.

Definition ctextendAll (c1 : context) (types : list (typeId * kind)) :=
  fold_left (
    fun c2 (type : typeId * kind) =>
      match type with
      | (a, k2) => ctextend c2 a k2
      end
  ) types c1.

Fixpoint lookupEVar c1 e :=
  match e with
  | evar x1 =>
    match c1 with
    | cempty => None
    | ceextend c2 x2 t =>
      match eqIdDec x1 x2 with
      | left _ => Some t
      | right _ => lookupEVar c2 e
      end
    | ctextend c2 _ _ => lookupEVar c2 e
    end
  | _ => None
  end.

Fixpoint lookupTVar c1 t :=
  match t with
  | tvar a1 =>
    match c1 with
    | cempty => None
    | ceextend c2 _ _ => lookupTVar c2 t
    | ctextend c2 a2 k =>
      match eqIdDec a1 a2 with
      | left _ => Some k
      | right _ => lookupTVar c2 t
      end
    end
  | _ => None
  end.

Fixpoint freeTVarsInType (t1 : type) :=
  match t1 with
  | tptwithr pt r =>
    match pt with
    | ptarrow t2 t3 => freeTVarsInType t2 ++ freeTVarsInType t3
    | ptforall a k t2 =>
      freeTVarsInKind k ++ (remove eqIdDec a (freeTVarsInType t2))
    end
  | trow r => freeTVarsInRow r
  | tvar a => a :: nil
  | tabs a k t3 => freeTVarsInKind k ++ (remove eqIdDec a (freeTVarsInType t3))
  | tapp t2 t3 => freeTVarsInType t2 ++ freeTVarsInType t3
  end

with freeTVarsInRow (r : row) :=
  match r with
  | rempty => nil
  | rsingleton t => freeTVarsInType t
  | runion r1 r2 => freeTVarsInRow r1 ++ freeTVarsInRow r2
  end

with freeTVarsInKind (k1 : kind) :=
  match k1 with
  | ktype => nil
  | krow => nil
  | keffect a x t => remove eqIdDec a (freeTVarsInType t)
  | karrow a k2 k3 =>
    freeTVarsInKind k2 ++ remove eqIdDec a (freeTVarsInKind k3)
  end.

Definition occursInType (a : typeId) (t : type) :=
  match in_dec eqIdDec a (freeTVarsInType t) with
  | left _ => true
  | right _ => false
  end.

(* TODO: make substitution capture-avoiding *)
Fixpoint substTypeInType (t1 : type) (a1 : typeId) (t2 : type) :=
  match t1 with
  | tptwithr pt r =>
    match pt with
    | ptarrow t3 t4 =>
      tptwithr (
        ptarrow
          (substTypeInType t3 a1 t2)
          (substTypeInType t4 a1 t2)
      ) (substTypeInRow r a1 t2)
    | ptforall a2 k t3 =>
      tptwithr (
        ptforall a2 (substTypeInKind k a1 t2) (
          match eqIdDec a1 a2 with
          | left _ => t3
          | right _ => substTypeInType t3 a1 t2
          end
        )
      ) (substTypeInRow r a1 t2)
    end
  | trow r => trow (substTypeInRow r a1 t2)
  | tvar a2 =>
    match eqIdDec a1 a2 with
    | left _ => t2
    | right _ => tvar a2
    end
  | tabs a2 k t3 =>
    match eqIdDec a1 a2 with
    | left _ => tabs a2 (substTypeInKind k a1 t2) t3
    | right _ => tabs a2 (substTypeInKind k a1 t2) (substTypeInType t3 a1 t2)
    end
  | tapp t3 t4 => tapp (substTypeInType t3 a1 t2) (substTypeInType t4 a1 t2)
  end

with substTypeInRow (r1 : row) (a : typeId) (t1 : type) :=
  match r1 with
  | rempty => rempty
  | rsingleton t2 => rsingleton (substTypeInType t2 a t1)
  | runion r2 r3 => runion (substTypeInRow r2 a t1) (substTypeInRow r3 a t1)
  end

with substTypeInKind (k1 : kind) (a1 : typeId) (t1 : type) :=
  match k1 with
  | ktype => ktype
  | krow => krow
  | keffect a2 x t2 =>
    match eqIdDec a1 a2 with
    | left _ => keffect a2 x t2
    | right _ => keffect a2 x (substTypeInType t2 a1 t1)
    end
  | karrow a2 k2 k3 =>
    match eqIdDec a1 a2 with
    | left _ => karrow a2 (substTypeInKind k2 a1 t1) k3
    | right _ =>
      karrow a2 (substTypeInKind k2 a1 t1) (substTypeInKind k3 a1 t1)
    end
  end.

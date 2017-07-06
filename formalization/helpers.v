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
    | ptunit => nil
    | ptarrow t2 t3 => freeTVarsInType t2 ++ freeTVarsInType t3
    end
  | trow r => freeTVarsInRow r
  | tvar a => a :: nil
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
    | ptunit => tptwithr ptunit (substTypeInRow r a1 t2)
    | ptarrow t3 t4 =>
      tptwithr (
        ptarrow
          (substTypeInType t3 a1 t2)
          (substTypeInType t4 a1 t2)
      ) (substTypeInRow r a1 t2)
    end
  | trow r => trow (substTypeInRow r a1 t2)
  | tvar a2 =>
    match eqIdDec a1 a2 with
    | left _ => t2
    | right _ => tvar a2
    end
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
  end.

(* TODO: make substitution capture-avoiding *)
Fixpoint substTermInTerm (e1 : term) (x1 : termId) (e2 : term) :=
  match e1 with
  | eunit => e1
  | evar x2 =>
    match eqIdDec x1 x2 with
    | left _ => e2
    | right _ => e1
    end
  | eabs x2 t e3 =>
    match eqIdDec x1 x2 with
    | left _ => e1
    | right _ => eabs x2 t (substTermInTerm e3 x1 e2)
    end
  | eappbv e3 e4 =>
    eappbv (substTermInTerm e3 x1 e2) (substTermInTerm e4 x1 e2)
  | eappbn e3 e4 =>
    eappbn (substTermInTerm e3 x1 e2) (substTermInTerm e4 x1 e2)
  | eeffect a1 (keffect a2 x2 t) e3 =>
    match eqIdDec x1 x2 with
    | left _ => e1
    | right _ => eeffect a1 (keffect a2 x2 t) (substTermInTerm e3 x1 e2)
    end
  | eprovide t x2 e3 e4 =>
    eprovide t x2 (substTermInTerm e3 x1 e2) (substTermInTerm e4 x1 e2)
  | _ => e1
  end.

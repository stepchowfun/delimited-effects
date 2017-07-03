Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import syntax.

Definition eqId {X : Set} (id1 : id X) (id2 : id X) : bool :=
  match id1 with
  | makeId _ s1 => match id2 with
    | makeId _ s2 => andb (prefix s1 s2) (prefix s2 s1)
    end
  end.

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
  | evar x1 => match c1 with
               | cempty => None
               | ceextend c2 x2 t => if eqId x1 x2
                                     then Some t
                                     else lookupEVar c2 e
               | ctextend c2 _ _ => lookupEVar c2 e
               end
  | _ => None
  end.

Fixpoint lookupTVar c1 t :=
  match t with
  | tvar a1 => match c1 with
               | cempty => None
               | ceextend c2 _ _ => lookupTVar c2 t
               | ctextend c2 a2 k => if eqId a1 a2
                                     then Some k
                                     else lookupTVar c2 t
               end
  | _ => None
  end.

Fixpoint occursType (a1 : typeId) (t1 : type) :=
  match t1 with
  | tptwithr pt r => orb (occursRow a1 r) (
                       match pt with
                       | ptarrow t2 t3 => orb
                                            (occursType a1 t2)
                                            (occursType a1 t3)
                       | ptforall a2 k t2 => if eqId a1 a2
                                             then occursKind a1 k
                                             else orb
                                                    (occursKind a1 k)
                                                    (occursType a1 t2)
                       end
                     )
  | trow r => occursRow a1 r
  | tvar a2 => eqId a1 a2
  | tabs a2 k t2 => if eqId a1 a2
                    then occursKind a1 k
                    else orb (occursKind a1 k) (occursType a1 t2)
  | tapp t2 t3 => orb (occursType a1 t2) (occursType a1 t3)
  end

with occursRow (a : typeId) (r1 : row) :=
  match r1 with
  | rempty => false
  | rsingleton t => occursType a t
  | runion r2 r3 => orb (occursRow a r2) (occursRow a r3)
  end

with occursKind (a1 : typeId) (k1 : kind) :=
  match k1 with
  | ktype => false
  | krow => false
  | keffect a2 x t => if eqId a1 a2
                      then false
                      else occursType a1 t
  | karrow a2 k2 k3 => if eqId a1 a2
                       then occursKind a1 k2
                       else orb (occursKind a1 k2) (occursKind a1 k3)
  end.

(* TODO: make substitution capture-avoiding *)
Fixpoint substType (t1 : type) (a1 : typeId) (t2 : type) :=
  match t1 with
  | tptwithr pt r => match pt with
                     | ptarrow t3 t4 => tptwithr (
                                          ptarrow
                                            (substType t3 a1 t2)
                                            (substType t4 a1 t2)
                                        ) (substRow r a1 t2)
                     | ptforall a2 k t3 => tptwithr (
                                             ptforall a2 (substKind k a1 t2) (
                                               if eqId a1 a2
                                               then t3
                                               else substType t3 a1 t2
                                             )
                                           ) (substRow r a1 t2)
                     end
  | trow r => trow (substRow r a1 t2)
  | tvar a2 => if eqId a1 a2
               then t2
               else tvar a2
  | tabs a2 k t3 => if eqId a1 a2
                    then tabs a2 (substKind k a1 t2) t3
                    else tabs a2 (substKind k a1 t2) (substType t3 a1 t2)
  | tapp t3 t4 => tapp (substType t3 a1 t2) (substType t4 a1 t2)
  end

with substRow (r1 : row) (a : typeId) (t1 : type) :=
  match r1 with
  | rempty => rempty
  | rsingleton t2 => rsingleton (substType t2 a t1)
  | runion r2 r3 => runion (substRow r2 a t1) (substRow r3 a t1)
  end

with substKind (k1 : kind) (a1 : typeId) (t1 : type) :=
  match k1 with
  | ktype => ktype
  | krow => krow
  | keffect a2 x t2 => if eqId a1 a2
                       then keffect a2 x t2
                       else keffect a2 x (substType t2 a1 t1)
  | karrow a2 k2 k3 => if eqId a1 a2
                       then karrow a2 (substKind k2 a1 t1) k3
                       else karrow a2
                              (substKind k2 a1 t1)
                              (substKind k3 a1 t1)
  end.

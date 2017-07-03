Require Import Coq.Lists.List.
Require Import Coq.Strings.String.
Require Import syntax.

Definition eqId {X : Set} (i1 : id X) (i2 : id X) : bool :=
  match i1 with
  | makeId _ s1 => match i2 with
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
  | evar i1 => match c1 with
               | cempty => None
               | ceextend c2 i2 t => if eqId i1 i2
                                     then Some t
                                     else lookupEVar c2 e
               | ctextend c2 _ _ => lookupEVar c2 e
               end
  | _ => None
  end.

Fixpoint lookupTVar c1 t :=
  match t with
  | tvar i1 => match c1 with
               | cempty => None
               | ceextend c2 _ _ => lookupTVar c2 t
               | ctextend c2 i2 k => if eqId i1 i2
                                     then Some k
                                     else lookupTVar c2 t
               end
  | _ => None
  end.

(* TODO: the occurs check should check inside rows and kinds *)
Fixpoint occurs (id1 : typeId) (t1 : type) :=
  match t1 with
  | tptwithx pt r => match pt with
                     | ptarrow t2 t3 => orb (occurs id1 t2) (occurs id1 t3)
                     | ptforall _ _ t2 => occurs id1 t2
                     end
  | trow r => false
  | tvar id2 => eqId id1 id2
  | tabs id2 _ t2 => if eqId id1 id2
                     then false
                     else occurs id1 t2
  | tapp t2 t3 => orb (occurs id1 t2) (occurs id1 t3)
  end.

(* TODO: substition should check inside rows and kinds, and be capture-avoiding *)
Fixpoint typeSubst (t1 : type) (id1 : typeId) (t2 : type) :=
  match t1 with
  | tptwithx pt r => match pt with
                   | ptarrow t3 t4 => tptwithx (
                                        ptarrow
                                          (typeSubst t3 id1 t2)
                                          (typeSubst t4 id1 t2)
                                      ) r
                   | ptforall id2 k t3 => tptwithx (
                                            ptforall id2 k
                                              (typeSubst t3 id1 t2)
                                          ) r
                   end
  | trow r => t1
  | tvar id2 => if eqId id1 id2
                then t2
                else (tvar id2)
  | tabs id2 k t3 => if eqId id1 id2
                     then t1
                     else tabs id2 k (typeSubst t3 id1 t2)
  | tapp t3 t4 => tapp (typeSubst t3 id1 t2) (typeSubst t4 id1 t2)
  end.

Require Import syntax.

(* Typing rules *)

Inductive hasType : context -> term -> type -> Prop :=
| tVar :
    forall e t c,
    lookupEVar c e = Some t ->
    hasType c e t
(* TODO: Fill in the other rules here. *)

(* Kinding rules *)

with hasKind : context -> type -> kind -> Prop :=
| kArrow :
    forall t1 t2 r c,
    hasKind c t1 ktype ->
    hasKind c t2 ktype ->
    hasKind c (trow r) krow ->
    hasKind c (tptwithx (ptarrow t1 t2) r) ktype
| kForAll :
    forall t a r k c,
    hasKind (ctextend c a k) t ktype ->
    hasKind c (trow r) krow ->
    hasKind c (tptwithx (ptforall a k t) r) ktype
| kEmpty :
    forall c,
    hasKind c (trow rempty) krow
| kSingleton :
    forall x t1 t2 a c,
    hasKind c t1 (keffect a x t2) ->
    hasKind c (trow (rsingleton t1)) krow
| kUnion :
    forall r1 r2 c,
    hasKind c (trow r1) krow ->
    hasKind c (trow r2) krow ->
    hasKind c (trow (runion r1 r2)) krow
| kVar :
    forall a k c,
    lookupTVar c a = Some k ->
    hasKind c a k
| kAbs :
    forall t a k1 k2 c,
    hasKind (ctextend c a k1) t k2 ->
    hasKind c (tabs a k1 t) (karrow a k1 k2)
| kApp :
    forall t1 t2 a k1 k2 c,
    hasKind c t1 k2 ->
    hasKind c t1 (karrow a k1 k2) ->
    hasKind c (tapp t1 t2) k2 (* TODO: substitute t1 for a in k2 *)

(* Operation type well-formedness *)

with opTypeWellFormed : type -> typeId -> Prop :=
| wfArrow :
    forall t1 t2 a r,
    opTypeWellFormed t2 a ->
    opTypeWellFormed (tptwithx (ptarrow t1 t2) r) a
| wfForAll :
    forall t a1 a2 r k,
    opTypeWellFormed t a2 ->
    eqId a1 a2 = false ->
    opTypeWellFormed (tptwithx (ptforall a1 k t) r) a2
| wfTWithEff :
    forall t a r,
    subtype (trow (rsingleton (tvar a))) (trow r) ->
    opTypeWellFormed (tptwithx t r) a

(* Subtyping *)

with subtype : type -> type -> Prop :=
| stRefl :
    forall t,
    subtype t t
| stTrans :
    forall t1 t2 t3,
    subtype t1 t2 ->
    subtype t2 t3 ->
    subtype t1 t3
| stArrow :
    forall t1 t2 t3 t4 r1 r2,
    subtype t3 t1 ->
    subtype t2 t4 ->
    subtype (trow r1) (trow r2) ->
    subtype (tptwithx (ptarrow t1 t2) r1) (tptwithx (ptarrow t3 t4) r2)
| stForAll :
    forall t1 t2 a r1 r2 k,
    subtype t1 t2 ->
    subtype (trow r1) (trow r2) ->
    subtype (tptwithx (ptforall a k t1) r1) (tptwithx (ptforall a k t2) r2)
| stEmpty :
    forall r,
    subtype (trow rempty) (trow r)
| stSingleton :
    forall t1 t2,
    subtype t1 t2 ->
    subtype (trow (rsingleton t1)) (trow (rsingleton t2))
| stUnion :
    forall t r1 r2,
    subtype (trow r1) t ->
    subtype (trow r2) t ->
    subtype (trow (runion r1 r2)) t
| stWeaken :
    forall r1 r2,
    subtype (trow r1) (trow (runion r1 r2))
| stExchange :
    forall r1 r2,
    subtype (trow (runion r1 r2)) (trow (runion r2 r1))
| stTypeAbs :
    forall t1 t2 a k,
    subtype t1 t2 ->
    subtype (tabs a k t1) (tabs a k t2)
| stTypeApp :
    forall t1 t2 t3,
    subtype t1 t2 ->
    subtype (tapp t1 t3) (tapp t2 t3).

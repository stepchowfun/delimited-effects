Require Import syntax.
Require Import helpers.

(* Effect row membership *)

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

(* Typing rules *)

Inductive hasType : context -> term -> type -> Prop :=
| tVar :
    forall e t c,
    lookupEVar c e = Some t ->
    hasType c e t
| htAbs :
    forall e x t1 t2 c,
    hasType (ceextend c x t1) e t2 ->
    hasKind c t1 ktype ->
    lookupEVar c (evar x) = None ->
    hasType c (eabs x t1 e) (tptwithr (ptarrow t1 t2) rempty)
| htAppByV :
    forall e1 e2 pt1 pt2 pt3 r1 r2 r3 r4 c,
    hasType c e1 (tptwithr pt1 r1) ->
    hasType c e2 (tptwithr (ptarrow (tptwithr pt2 r2) (tptwithr pt3 r3)) r4) ->
    subtype (tptwithr pt1 rempty) (tptwithr pt2 r2) ->
    hasType c (eappbv e2 e1) (tptwithr pt3 (runion (runion r1 r3) r4))
| htAppByN :
    forall e1 e2 pt1 pt2 pt3 r1 r2 r3 r4 c,
    hasType c e1 (tptwithr pt1 r1) ->
    hasType c e2 (tptwithr (ptarrow (tptwithr pt2 r2) (tptwithr pt3 r3)) r4) ->
    subtype (tptwithr pt1 r1) (tptwithr pt2 r2) ->
    hasType c (eappbn e2 e1) (tptwithr pt3 (runion r3 r4))
| htTypeAbs :
    forall e t a k c,
    hasType (ctextend c a k) e t ->
    lookupTVar c (tvar a) = None ->
    hasType c (etabs a k e) (tptwithr (ptforall a k t) rempty)
| htTypeApp :
    forall e t1 t2 a k c,
    hasType c e (tptwithr (ptforall a k t1) rempty) ->
    hasKind c t2 k ->
    hasType c (etapp e t2) (substTypeInType t1 a t2)
| htEffect :
    forall e x t1 t2 a1 a2s a3 c,
    opTypeWellFormed t1 a3 ->
    hasKind (ctextend (ctextendAll c a2s) a3 (keffect a3 x t1)) t1 ktype ->
    occursInType a1 t2 = false ->
    lookupTVar c (tvar a1) = None ->
    lookupEVar c (evar x) = None ->
    hasType (
      ceextend (ctextend c a1 (expandArgs a2s (keffect a3 x t1))) x t1
    ) e t2 ->
    hasType c (eeffect a1 (expandArgs a2s (keffect a3 x t1)) e) t2
| htProvide :
    forall e1 e2 x pt t1 t2 t3 a r1 r2 c,
    hasType c e1 t1 ->
    hasType c e2 (tptwithr pt r1) ->
    hasKind c t2 (keffect a x t3) ->
    subtype t1 (substTypeInType t3 a t2) ->
    (* TODO: ensure that r2 = r1 - {t2} *)
    hasType c (eprovide t2 x e1 e2) (tptwithr pt r2)
| htSub :
    forall c e t1 t2,
    hasType c e t1 ->
    subtype t1 t2 ->
    hasType c e t2

(* Kinding rules *)

with hasKind : context -> type -> kind -> Prop :=
| kArrow :
    forall t1 t2 r c,
    hasKind c t1 ktype ->
    hasKind c t2 ktype ->
    hasKind c (trow r) krow ->
    hasKind c (tptwithr (ptarrow t1 t2) r) ktype
| kForAll :
    forall t a r k c,
    hasKind (ctextend c a k) t ktype ->
    hasKind c (trow r) krow ->
    hasKind c (tptwithr (ptforall a k t) r) ktype
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
    hasKind c (tapp t1 t2) (substTypeInKind k2 a t1)

(* Operation type well-formedness *)

with opTypeWellFormed : type -> typeId -> Prop :=
| wfArrow :
    forall t1 t2 a r,
    opTypeWellFormed t2 a ->
    opTypeWellFormed (tptwithr (ptarrow t1 t2) r) a
| wfForAll :
    forall t a1 a2 r k h,
    opTypeWellFormed t a2 ->
    eqIdDec a1 a2 = right h ->
    opTypeWellFormed (tptwithr (ptforall a1 k t) r) a2
| wfTWithEff :
    forall t a r,
    subtype (trow (rsingleton (tvar a))) (trow r) ->
    opTypeWellFormed (tptwithr t r) a

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
    subtype (tptwithr (ptarrow t1 t2) r1) (tptwithr (ptarrow t3 t4) r2)
| stForAll :
    forall t1 t2 a r1 r2 k,
    subtype t1 t2 ->
    subtype (trow r1) (trow r2) ->
    subtype (tptwithr (ptforall a k t1) r1) (tptwithr (ptforall a k t2) r2)
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

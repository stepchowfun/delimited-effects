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
| tUnit :
    forall c,
    hasType c eunit (tptwithr ptunit rempty)
| tVar :
    forall e t c,
    lookupEVar c e = Some t ->
    hasType c e t
| htAbs :
    forall e x t1 t2 c,
    hasType (ceextend c x t1) e t2 ->
    hasKind c t1 ktype ->
    hasType c (eabs x t1 e) (tptwithr (ptarrow t1 t2) rempty)
| htApp :
    forall e1 e2 pt1 pt2 pt3 r1 r2 r3 r4 c,
    hasType c e1 (tptwithr pt1 r1) ->
    hasType c e2 (tptwithr (ptarrow (tptwithr pt2 r2) (tptwithr pt3 r3)) r4) ->
    subtype (tptwithr pt1 r1) (tptwithr pt2 r2) ->
    hasType c (eapp e2 e1) (tptwithr pt3 (runion r3 r4))
| htProvide :
    forall e1 e2 x pt t1 t2 t3 a r1 r2 c,
    hasType c e1 t1 ->
    hasType c e2 (tptwithr pt r1) ->
    hasKind c t2 (keffect a x t3) ->
    subtype t1 (substTypeInType t3 a t2) ->
    ~(rowContains r2 t2) ->
    subtype (trow (runion (rsingleton t2) r2)) (trow r1) ->
    subtype (trow r1) (trow (runion (rsingleton t2) r2)) ->
    hasType c (eprovide t2 x e1 e2) (tptwithr pt r2)
| htSub :
    forall c e t1 t2,
    hasType c e t1 ->
    subtype t1 t2 ->
    hasType c e t2

(* Kinding rules *)

with hasKind : context -> type -> kind -> Prop :=
| kUnit :
    forall r c,
    hasKind c (trow r) krow ->
    hasKind c (tptwithr ptunit r) ktype
| kArrow :
    forall t1 t2 r c,
    hasKind c t1 ktype ->
    hasKind c t2 ktype ->
    hasKind c (trow r) krow ->
    hasKind c (tptwithr (ptarrow t1 t2) r) ktype
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

(* Operation type well-formedness *)

with opTypeWellFormed : type -> typeId -> Prop :=
| wfArrow :
    forall t1 t2 a r,
    opTypeWellFormed t2 a ->
    opTypeWellFormed (tptwithr (ptarrow t1 t2) r) a
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
| stEmpty :
    forall r,
    subtype (trow rempty) (trow r)
| stUnion :
    forall t r1 r2,
    subtype (trow r1) t ->
    subtype (trow r2) t ->
    subtype (trow (runion r1 r2)) t
| stWeakenLeft :
    forall r1 r2,
    subtype (trow r1) (trow (runion r1 r2))
| stWeakenRight :
    forall r1 r2,
    subtype (trow r2) (trow (runion r1 r2)).

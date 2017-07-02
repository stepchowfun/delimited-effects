Require Import syntax.

(* Typing rules *)

Inductive hasType : context -> term -> type -> Prop :=
| tVar :
    forall c e t,
    lookupEVar c e = Some t ->
    hasType c e t
(* TODO: Fill in the other rules here. *)

(* Kinding rules *)

with hasKind : context -> type -> kind -> Prop :=
| kArrow :
    forall c t1 t2 r,
    hasKind c t1 ktype ->
    hasKind c t2 ktype ->
    hasKind c (trow r) krow ->
    hasKind c (tptwithx (ptarrow t1 t2) r) ktype
(* TODO: Fill in the other rules here. *)

(* Operation type well-formedness *)

with opTypeWellFormed : type -> typeId -> Prop :=
| wfArrow :
    forall t1 t2 a r,
    opTypeWellFormed t2 a ->
    opTypeWellFormed (tptwithx (ptarrow t1 t2) r) a
| wfForAll :
    forall t k r a1 a2,
    opTypeWellFormed t a2 ->
    eqId a1 a2 = false ->
    opTypeWellFormed (tptwithx (ptforall a1 k t) r) a2
| wfTWithEff :
    forall a r t,
    subsumes (rsingleton (tvar a)) r ->
    opTypeWellFormed (tptwithx t r) a

(* Effect row subsumption *)

with subsumes : row -> row -> Prop :=
| rTrans :
    forall r1 r2 r3,
    subsumes r1 r2 ->
    subsumes r2 r3 ->
    subsumes r1 r3
| rEmpty:
    forall r1,
    subsumes rempty r1
| rSingleton :
    forall t1 t2,
    subtype t1 t2 ->
    subsumes (rsingleton t1) (rsingleton t2)
| rUnion :
    forall r1 r2 r3,
    subsumes r1 r3 ->
    subsumes r2 r3 ->
    subsumes (runion r1 r2) r3
| rWeaken :
    forall r1 r2,
    subsumes r1 (runion r1 r2)
| rExchange :
    forall r1 r2,
    subsumes (runion r1 r2) (runion r2 r1)

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
    subsumes r1 r2 ->
    subtype (tptwithx (ptarrow t1 t2) r1) (tptwithx (ptarrow t3 t4) r2)
| stForAll :
    forall t1 t2 r1 r2 a k,
    subtype t1 t2 ->
    subsumes r1 r2 ->
    subtype (tptwithx (ptforall a k t1) r1) (tptwithx (ptforall a k t2) r2)
| stTypeAbs :
    forall t1 t2 a k,
    subtype t1 t2 ->
    subtype (tabs a k t1) (tabs a k t2)
| stTypeApp :
    forall t1 t2 t3,
    subtype t1 t2 ->
    subtype (tapp t1 t3) (tapp t2 t3).

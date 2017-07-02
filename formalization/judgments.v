Require Import syntax.

(* Typing rules *)

Inductive hasType : context -> term -> scheme -> Prop :=
| tVar :
    forall c e s,
    lookupEVar c e = Some s ->
    hasType c e s
(* TODO: Fill in the other rules here. *)

(* Kinding rules *)

with hasKind : context -> scheme -> kind -> Prop :=
| kArrow :
    forall c s1 s2 r,
    hasKind c s1 ktype ->
    hasKind c s2 ktype ->
    hasKind c (srow r) krow ->
    hasKind c (stwithx (tarrow s1 s2) r) ktype
(* TODO: Fill in the other rules here. *)

(* Operation type well-formedness *)

with opTypeWellFormed : scheme -> schemeId -> Prop :=
| wfArrow :
    forall s1 s2 a r,
    opTypeWellFormed s2 a ->
    opTypeWellFormed (stwithx (tarrow s1 s2) r) a
| wfForAll :
    forall s k r a1 a2,
    opTypeWellFormed s a2 ->
    eqId a1 a2 = false ->
    opTypeWellFormed (stwithx (tforall a1 k s) r) a2
| wfTWithEff :
    forall a r t,
    subsumes (rsingleton (svar a)) r ->
    opTypeWellFormed (stwithx t r) a

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
    forall s1 s2,
    subtype s1 s2 ->
    subsumes (rsingleton s1) (rsingleton s2)
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

with subtype : scheme -> scheme -> Prop :=
| stRefl :
    forall s,
    subtype s s
| stTrans :
    forall s1 s2 s3,
    subtype s1 s2 ->
    subtype s2 s3 ->
    subtype s1 s3
| stArrow :
    forall s1 s2 s3 s4 r1 r2,
    subtype s3 s1 ->
    subtype s2 s4 ->
    subsumes r1 r2 ->
    subtype (stwithx (tarrow s1 s2) r1) (stwithx (tarrow s3 s4) r2)
| stForAll :
    forall s1 s2 r1 r2 a k,
    subtype s1 s2 ->
    subsumes r1 r2 ->
    subtype (stwithx (tforall a k s1) r1) (stwithx (tforall a k s2) r2)
| stSchemeAbs :
    forall s1 s2 a k,
    subtype s1 s2 ->
    subtype (sabs a k s1) (sabs a k s2)
| stSchemeApp :
    forall s1 s2 s3,
    subtype s1 s2 ->
    subtype (sapp s1 s3) (sapp s2 s3).

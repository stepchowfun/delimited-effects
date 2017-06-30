Require Import syntax.

(* Effect row subsumption *)

Inductive subsumes : row -> row -> Prop :=
| rsRefl :
    forall r,
    subsumes r r
| rsTrans :
    forall r1 r2 r3,
    subsumes r1 r2 ->
    subsumes r2 r3 ->
    subsumes r1 r3
| rsContract:
    forall r1,
    subsumes (runion r1 r1) r1
| rsWeaken :
    forall r1 r2,
    subsumes r1 (runion r1 r2)
| rsExchange :
    forall r1 r2,
    subsumes (runion r1 r2) (runion r2 r1)
| rsSub :
    forall r1 r2 r3,
    subsumes r1 r2 ->
    subsumes (runion r1 r3) (runion r2 r3)
| rsId:
    forall r1,
    subsumes rempty r1
| rsAssoc:
    forall r1 r2 r3,
    subsumes (runion r1 r2) r3 ->
    subsumes r1 (runion r2 r3)
| rsSchemeEq :
    forall s1 s2,
    schemeEq s1 s2 ->
    subsumes (rsingleton s1) (rsingleton s2)

(* Subtyping *)

with isSubtypeOf : scheme -> scheme -> Prop :=
| stRefl :
    forall s,
    isSubtypeOf s s
| stTrans :
    forall s1 s2 s3,
    isSubtypeOf s1 s2 ->
    isSubtypeOf s2 s3 ->
    isSubtypeOf s1 s3
| stArrow :
    forall s1 s2 s3 s4 r1 r2,
    isSubtypeOf s3 s1 ->
    isSubtypeOf s2 s4 ->
    subsumes r1 r2 ->
    isSubtypeOf (stwithx (tarrow s1 s2) r1) (stwithx (tarrow s3 s4) r2)

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
    opTypeWellFormed (stwithx (ttforall a1 k s) r) a2
| wfTWithEff :
    forall a r t,
    subsumes (rsingleton (svar a)) r ->
    opTypeWellFormed (stwithx t r) a

(* Typing rules *)

with hasType : context -> term -> scheme -> Prop :=
| htVar :
    forall c e s,
    lookupEVar c e = Some s ->
    hasType c e s
(* TODO: Fill in the other rules here. *)

(* Kinding rules *)

with hasKind : context -> scheme -> kind -> Prop :=
| hkArrow :
    forall c s1 s2 r,
    hasKind c s1 ktype ->
    hasKind c s2 ktype ->
    hasKind c (srow r) krow ->
    hasKind c (stwithx (tarrow s1 s2) r) ktype
(* TODO: Fill in the other rules here. *)

(* Scheme equivalence *)

with schemeEq : scheme -> scheme -> Prop :=
| seRefl :
    forall s,
    schemeEq s s
| seSymm :
    forall s1 s2,
    schemeEq s1 s2 ->
    schemeEq s2 s1
| seTrans :
    forall s1 s2 s3,
    schemeEq s1 s2 ->
    schemeEq s2 s3 ->
    schemeEq s1 s3
(* TODO: Fill in the other rules here. *)

(* Kind equivalence *)

with kindEq : kind -> kind -> Prop :=
| keRefl :
    forall k,
    kindEq k k.
(* TODO: Fill in the other rules here. *)

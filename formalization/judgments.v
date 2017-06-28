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
| rsSub :
    forall r1 r2 r3,
    subsumes r1 r2 ->
    subsumes (runion r1 r3) (runion r2 r3)
| rsLeftAssoc:
    forall r1 r2 r3,
    subsumes (runion r1 r2) r3 ->
    subsumes r1 (runion r2 r3)
| rsRightAssoc:
    forall r1 r2 r3,
    subsumes r1 (runion r2 r3) ->
    subsumes (runion r1 r2) r3
| rsLeftContract:
    forall r1,
    subsumes (runion r1 r1) r1
| rsRightContract:
    forall r1,
    subsumes r1 (runion r1 r1)
| rsWeaken :
    forall r1 r2,
    subsumes r1 (runion r1 r2)
| rsLeftId:
    forall r1,
    subsumes (runion rempty r1) r1
| rsRightId:
    forall r1,
    subsumes r1 (runion rempty r1)
| rsExchange :
    forall r1 r2,
    subsumes (runion r1 r2) (runion r2 r1)
| rsSchemeEq :
    forall s1 s2,
    schemeEq s1 s2 ->
    subsumes (rsingleton s1) (rsingleton s2)

(* Subtyping *)

with isSubtypeOf : scheme -> scheme -> Prop :=
| stRefl :
    forall s,
    isSubtypeOf s s
(* TODO: Fill in the other rules here. *)

(* Operation type well-formedness *)

with opTypeWellFormed : scheme -> schemeId -> Prop :=
| wfArrow :
    forall s1 s2 a r,
    opTypeWellFormed s2 a ->
    opTypeWellFormed (stwithx (tarrow s1 s2) r) a
(* TODO: Fill in the other rules here. *)

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
    forall a b,
    schemeEq a b
(* TODO: Fill in the other rules here. *)

(* Kind equivalence *)

with kindEq : kind -> kind -> Prop :=
| keRefl :
    forall a b,
    kindEq a b.
(* TODO: Fill in the other rules here. *)

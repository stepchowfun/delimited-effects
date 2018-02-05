(*******************************************)
(*******************************************)
(****                                   ****)
(****   Names with decidable equality   ****)
(****                                   ****)
(*******************************************)
(*******************************************)

(* Term variables *)

Parameter eName : Set.
Parameter eNameEq : forall x1 x2 : eName, { x1 = x2 } + { x1 <> x2 }.

(* Type variables *)

Parameter tName : Set.
Parameter tNameEq : forall x1 x2 : tName, { x1 = x2 } + { x1 <> x2 }.

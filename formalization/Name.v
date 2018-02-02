(*******************************************)
(*******************************************)
(****                                   ****)
(****   Names with decidable equality   ****)
(****                                   ****)
(*******************************************)
(*******************************************)

Parameter name : Set.
Parameter nameEq : forall x1 x2 : name, { x1 = x2 } + { x1 <> x2 }.

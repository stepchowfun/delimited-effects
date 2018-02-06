(*******************************************)
(*******************************************)
(****                                   ****)
(****   Names with decidable equality   ****)
(****                                   ****)
(*******************************************)
(*******************************************)

Require Import Main.Tactics.

Module Type NameSig.

(* Term variables *)

  Parameter eName : Set.
  Axiom eNameEq : forall x1 x2 : eName, { x1 = x2 } + { x1 <> x2 }.

  (* Type variables *)

  Parameter tName : Set.
  Axiom tNameEq : forall x1 x2 : tName, { x1 = x2 } + { x1 <> x2 }.

End NameSig.

Module Name : NameSig.

  Definition eName := nat.
  Theorem eNameEq : forall x1 x2 : nat, { x1 = x2 } + { x1 <> x2 }.
    intro.
    induction x1; intro; destruct x2; magic.
    specialize (IHx1 x2).
    magic.
  Defined.

  Definition tName := eName.
  Definition tNameEq := eNameEq.

End Name.

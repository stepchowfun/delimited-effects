let apply = \f -> f : forall a b . (a -> b) -> a -> b in
let double = \x -> x + x : Int -> Int in
let twice = \(f : Int -> Int) (x : Int) -> f (f x) in
apply twice double 4

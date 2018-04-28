let twice = \(f : Int -> Int) (x : Int) -> f (f x) in
let add = \(x : Int) (y : Int) -> x + y in
let double = \(x : Int) -> add x x in
twice double 4

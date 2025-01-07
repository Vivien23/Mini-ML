let f = fun x y z -> x + if true then y else z in
prInt (f (4+5) (let f a b = f a b b in f 1 2) (f 0 8 4))

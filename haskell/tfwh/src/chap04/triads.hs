

divisors x = [d | d <- [2..x-1], x `mod` d == 0]

disjoint xs ys = undefined

coprime x y = disjoint (divisors x) (divisors y)

triads n = [(x,y,z) | x <- [1..n], y <- [x+1 .. n]
                    , coprime x y
                    , z <- [y+1 .. n], x * x+ y * y == z * z]


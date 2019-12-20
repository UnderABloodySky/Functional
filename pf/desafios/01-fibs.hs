
take 4 (1 : 1 : zipWith (+) fibs (tails fibs))

1 : take 3 (1 : zipWith (+) fibs (tail fibs))

1 : 1 : take 2 (zipWith (+) fibs (tail fibs))

1 : 1 : take 2 (zipWith (+) (1 : 1 : zipWith (+) fibs (tail fibs)) (tail fibs))

1 : 1 : take 2 (zipWith (+) (1 : 1 : zipWith (+) fibs (tail fibs)) (1 : zipWith (+) fibs (tail fibs)))

1 : 1 : take 2 ((+) 1 1 : zipWith (+) (1 : zipWith (+) fibs (tail fibs)) (zipWith (+) fibs (tail fibs)))

1 : 1 : take 2 (2 : zipWith (+) (1 : zipWith (+) fibs (tail fibs)) (zipWith (+) fibs (tail fibs)))

1 : 1 : 2 : take 1 (zipWith (+) (1 : zipWith (+) fibs (tail fibs)) (zipWith (+) fibs (tail fibs)))

1 : 1 : 2 : take 1 (zipWith (+) (1 : zipWith (+) fibs (tail fibs)) (zipWith (+) (1 : 1 : zipWith (+) fibs (tail fibs)) (tail fibs)))

1 : 1 : 2 : take 1 (zipWith (+) (1 : zipWith (+) fibs (tail fibs)) (zipWith (+) (1 : 1 : zipWith (+) fibs (tail fibs)) (1 : zipWith (+) fibs (tail fibs))))

1 : 1 : 2 : take 1 (zipWith (+) (1 : zipWith (+) fibs (tail fibs)) ((+) 1 1 : zipWith (+) (1 : zipWith (+) fibs (tail fibs)) (zipWith (+) fibs (tail fibs))))

1 : 1 : 2 : take 1 (zipWith (+) (1 : zipWith (+) fibs (tail fibs)) (2 : zipWith (+) (1 : zipWith (+) fibs (tail fibs)) (zipWith (+) fibs (tail fibs))))

1 : 1 : 2 : take 1 (zipWith (+) (1 : zipWith (+) fibs (tail fibs)) (2 : zipWith (+) (1 : zipWith (+) fibs (tail fibs)) (zipWith (+) fibs (tail fibs))))

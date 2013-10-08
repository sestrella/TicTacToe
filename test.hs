main = do
    let x = multiplesOfThreeAndFive 1000
    putStrLn (show (sum x))

multiplesOfThreeAndFive n = [ x | x <- [1..n], x < 10 && (x `mod` 3 == 0 || x `mod` 5 == 0)  ]

--Nome: Iuri Bernardo Picolini Moro

add10toall :: [Int] -> [Int]
add10toall list = [x+10 | x <- list]

multN :: Int -> [Int] -> [Int]
multN num list = [x*num | x <- list]

multN' :: Int -> [Int] -> [Int]
multN' num list = map (\element -> element * num) list

applyExpr :: [Int] -> [Int]
applyExpr list = [3*x+2 | x <- list]

applyExpr' :: [Int] -> [Int]
applyExpr' list = map (\element -> 3 * element + 2) list

addSuffix :: String -> [String] -> [String]
addSuffix suffix list = [x ++ suffix | x <- list]

selectgt5 :: [Int] -> [Int]
selectgt5 list = [x | x <- list, x > 5]

sumOdds :: [Int] -> Int
sumOdds list = sum ([x | x <- list, x `mod` 2 == 1])

sumOdds' :: [Int] -> Int
sumOdds' list = sum (filter (\element -> element `mod` 2 == 1) list)

selectExpr :: [Int] -> [Int]
selectExpr list = [x | x <- list, x >= 20, x <= 50, x `mod` 2 == 0]

countShorts :: [String] -> Int
countShorts list = length ([x | x <- list, length (x) < 5])

calcExpr :: [Float] -> [Float]
calcExpr list = [x | x <- [x^2/2 | x <- list], x > 10]

trSpaces :: String -> String
trSpaces list = [if x == ' ' then '-' else x | x <- list]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd tuples = [snd x | x <- tuples]

dotProd :: [Int] -> [Int] -> Int
dotProd list1 list2 = sum([fst x * snd x | x <- (zip list1 list2)])
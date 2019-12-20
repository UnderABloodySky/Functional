

-- getDiagonals' :: [String] -> Int -> [Diagonal]
-- getDiagonals' xs l = let makeIndexOf = (\(ss,n) xs -> toCell (ss !! n) : xs)
--                         foldTTTT    = foldr makeIndexOf []
--                         indexes     = [0..l]
--                         zipCases    = (\f -> zip (f xs) indexes)
--                         getDiagonal = (\f -> foldTTTT (zipCases f))
--                     in
--                       (getDiagonal id) : (getDiagonal reverse) : []

-- ["XOXO", "...X", "X...", "OOOO"]

getDiagonal :: [String] -> String
getDiagonal xs = snd (foldr (\x (m, n) -> (m + 1, ((!!) x m) : n)) (0, []) xs)



["XOXX","XOXX","TXOO","OOOX"]

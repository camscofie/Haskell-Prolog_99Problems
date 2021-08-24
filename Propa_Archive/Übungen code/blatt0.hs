max31 :: Int -> Int -> Int -> Int
max31 x y z = 
   if x >= y && x >= z
   then x
   else max y z

max32 :: Int -> Int -> Int -> Int
max32 x y z
   | x >=y && x >= z = x
   | otherwise = max y z


max33 :: Int -> Int -> Int -> Int
max33 x y z = max x ( max z y)


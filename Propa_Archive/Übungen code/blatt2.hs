type Polynom = [Double]


cmult :: Polynom -> Double -> Polynom
cmult poly d = map (\x -> x*d) poly

eval :: Polynom -> Double -> Double
eval poly x = foldr (\a b -> a + b*x) 0 poly 

deriv :: Polynom -> Polynom
deriv [] = []
deriv poly = zipWith (*) [0..]  poly 

main = do
    print (cmult [23.3,3,2,4.2] 2)
    print (eval [2,3,4] 2)
    print (eval [1,2,3,4] 2)
    print (deriv [0,1,2,4])
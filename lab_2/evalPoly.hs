-- Evaluation of polynomials --
 
evalPoly :: Int -> Poly -> Int
-- evalPoly [] = []
evalPoly [p] _ = p
evalPoly (p:ps) x = p + (x * (evalPoly ps x))

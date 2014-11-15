-- Sum of polynomials --

type Poly = [Integer]
sumPoly :: Poly -> Poly -> Poly
sumPoly [] p = p
sumPoly p [] = p
sumPoly (p:ps) (q:qs) = (p+q):(sumPoly ps qs)

-- Evaluation of polynomials --

evalPoly :: Integer -> Poly -> Integer
-- evalPoly [] = []
evalPoly _ [p] = p
evalPoly x (p:ps) = p + (x * (evalPoly x ps)) 

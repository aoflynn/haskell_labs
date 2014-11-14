
triangleArea :: Float -> Float -> Float -> Float
triangleArea x y z = let s = (x + y + z) / 2
					 in sqrt(s*(s-x)*(s-y)*(s-z))
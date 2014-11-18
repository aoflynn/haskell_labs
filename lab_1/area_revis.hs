triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = if( a + b < c || a + c < b || b + c < a)
						then error "Not a triangle!!"
			 			else	let s = (a + b + c) / 2
								in sqrt(s*(s-a)*(s-b)*(s-c))

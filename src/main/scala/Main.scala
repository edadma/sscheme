package ca.hyperreal.sscheme


object Main extends App
{
	val env = environment( """
		(define divisors
			(lambda (n)
				(let f ((i 2))
				(cond
					((>= i n) '())
					((integer? (/ n i))
						(cons i (f (+ i 1))))
					(else (f (+ i 1))))))) """ )
	
	println( interpret( """ [divisors 5] """, env ) )
	println( interpret( """ [divisors 32] """, env ) )
}
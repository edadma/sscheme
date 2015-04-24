package ca.hyperreal.sscheme


object Main extends App
{
	val env = environment(
		"""
		(define lengthx
			(lambda (ls)
				(let loop ((ls ls) (n 0))
				(if (null? ls)
					n
					(loop (cdr ls) (+ n 1))))))
		""" )
	
	println( interpret( """ [lengthx '[]] """, env ) )
	println( interpret( """ [lengthx '[a b c]] """, env ) )
}
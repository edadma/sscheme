package ca.hyperreal.sscheme


object Main extends App
{
	val env = environment(
		"""
		(define sort #f)
		(let ()
			(define dosort
				(lambda (pred? ls n)
					(if (= n 1)
						(list (car ls))
						(let ((i (quotient n 2)))
							(merge pred?
								(dosort pred? ls i)
								(dosort pred? (list-tail ls i) (- n i)))))))
			(define merge
				(lambda (pred? l1 l2)
					(cond
						((null? l1) l2)
						((null? l2) l1)
						((pred? (car l2) (car l1))
						(cons (car l2) (merge pred? l1 (cdr l2))))
						(else (cons (car l1) (merge pred? (cdr l1) l2))))))
			(set! sort
				(lambda (pred? l)
					(if (null? l) l (dosort pred? l (length l))))))
		""" )
	
	println( interpret( """ (sort > '(3 4 2 1 2 5)) """, env ) )
}
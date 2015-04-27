package ca.hyperreal.sscheme

import org.scalatest._
import prop.PropertyChecks


class Examples extends FreeSpec with PropertyChecks with Matchers
{
	"sort" in
	{
	val env = environment( """
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
	
		interpret( """ (sort < l) """, env add ('l -> SList(5, 7, 3, 9, 2, 1, 6)) ) shouldBe SList(1, 2, 3, 5, 6, 7, 9)
		interpret( """ (sort > l) """, env add ('l -> SList(5, 7, 3, 9, 2, 1, 6)) ) shouldBe SList(9, 7, 6, 5, 3, 2, 1)
	}
}
package ca.hyperreal.sscheme

import math._

import ca.hyperreal.lia._

import org.scalatest._
import prop.PropertyChecks


class Tests extends FreeSpec with PropertyChecks with Matchers
{
	"parsing" in
	{
		a [RuntimeException] should be thrownBy {interpret( """ (= 1 1] """ )}
	}
	
	"primitives" in
	{
		interpret( """ (cdr '(a)) """ ) shouldBe SNil
		interpret( """ (cdr '(a b c)) """ ) shouldBe SList( 'b, 'c )
		interpret( """ (cdr (cons 3 '(4))) """ ) shouldBe SList(4)
		
		interpret( """ [- 1] """ ) shouldBe -1
		interpret( """ [- 10 1 1] """ ) shouldBe 8
		
		interpret( """ [/ 2] """ ) shouldBe Rational( 1, 2 )
		interpret( """ [/ 12 2 2] """ ) shouldBe 3
		
		interpret( """ (quotient 45 6) """ ) shouldBe 7
		
		interpret( """ [= 1 1] """ ) shouldBe true
		interpret( """ [= 1 2] """ ) shouldBe false
		interpret( """ [< 1 2] """ ) shouldBe true

		interpret( """ [integer? 1] """ ) shouldBe true
		interpret( """ [integer? 2.1] """ ) shouldBe false

		interpret( """ [sqrt 25] """ ) shouldBe 5
		interpret( """ [sqrt 2] """ ) shouldBe (sqrt( 2 ))
		
		interpret( """ [if #f "yes" "no"] """ ) shouldBe "no"
		interpret( """ [if #t "yes" "no"] """ ) shouldBe "yes"
		
		interpret( """ [[lambda [x y] [+ x y]] 3 4] """ ) shouldBe 7
		interpret( """ [[lambda x x] 3] """ ) shouldBe SList( 3 )
		
		interpret( """ [define f [lambda [x y] [+ [* x x] [* y y]]]] [define a 5] [f a 4] """ ) shouldBe 41
		interpret( """ [define x 5] [set! x 6] [+ x 1] """ ) shouldBe 7
 		
		interpret( """ (or (< 3 2) (> 3 4)) """ ) shouldBe false
		interpret( """ (or (< 5 2) (> 5 4)) """ ) shouldBe true
 		
		interpret( """ (and (> 3 2) (< 3 4)) """ ) shouldBe true
		interpret( """ (and (> 5 2) (< 5 4)) """ ) shouldBe false
 		
		interpret( """ (not #f) """ ) shouldBe true
		interpret( """ (not #t) """ ) shouldBe false
 		
		interpret( """ (let ((x (* 3 3)) (y (* 4 4))) (sqrt (+ x y))) """ ) shouldBe 5
		interpret( """ (let ((x 'a) (y '(b c))) (cons x y)) """ ) shouldBe SList( 'a, 'b, 'c )
		interpret( """
			(let ((x 0) (y 1))
				(let ((x y) (y x))
					(list x y))) """ ) shouldBe SList( 1, 0 )
		
		interpret( """ (length '()) """ ) shouldBe 0
		interpret( """ (length '(a b c)) """ ) shouldBe 3
	}
	
	"conditional" in
	{
	val env = environment( """
		[define test [lambda [x]
			[cond
				[[< x 5] "small"]
				[[< x 10] "medium"]
				[else "large"]]]] """ )
				
		interpret( """ [test 3] """, env ) shouldBe "small"
		interpret( """ [test 6] """, env ) shouldBe "medium"
		interpret( """ [test 15] """, env ) shouldBe "large"
	}
	
	"named let" in
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
	
		interpret( """ [divisors 5] """, env ) shouldBe SList()
		interpret( """ [divisors 32] """, env ) shouldBe SList( 2, 4, 8, 16 )
	}
	
	"pre-defined" in
	{
		interpret( """ [null? '[]] """ ) shouldBe true
		interpret( """ [null? 5] """ ) shouldBe false
		interpret( """ [null? '[1 2]] """ ) shouldBe false
		
		interpret( """ (boolean? #t) """ ) shouldBe true
		interpret( """ (boolean? #f) """ ) shouldBe true
		interpret( """ (boolean? 't) """ ) shouldBe false
		interpret( """ (boolean? '()) """ ) shouldBe false
	}
	
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
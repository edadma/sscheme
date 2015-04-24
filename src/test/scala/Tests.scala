package ca.hyperreal.sscheme

import math._

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
		interpret( """ [[lambda x x] 3] """ ) shouldBe List( 3 )
		
		interpret( """ [define f [lambda [x y] [+ [* x x] [* y y]]]] [define a 5] [f a 4] """ ) shouldBe 41
		interpret( """ [define x 5] [set! x 6] [+ x 1] """ ) shouldBe 7
 		
		interpret( """ (or (< 3 2) (> 3 4)) """ ) shouldBe false
		interpret( """ (or (< 5 2) (> 5 4)) """ ) shouldBe true
 		
		interpret( """ (and (> 3 2) (< 3 4)) """ ) shouldBe true
		interpret( """ (and (> 5 2) (< 5 4)) """ ) shouldBe false
 		
		interpret( """ (not #f) """ ) shouldBe true
		interpret( """ (not #t) """ ) shouldBe false
 		
		interpret( """ (let ((x (* 3 3)) (y (* 4 4))) (sqrt (+ x y))) """ ) shouldBe 5
		interpret( """ (let ((x 'a) (y '(b c))) (cons x y)) """ ) shouldBe List( 'a, 'b, 'c )
		interpret( """
			(let ((x 0) (y 1))
				(let ((x y) (y x))
					(list x y))) """ ) shouldBe List( 1, 0 )
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
}
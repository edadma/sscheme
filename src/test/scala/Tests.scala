package ca.hyperreal.sscheme

import org.scalatest._
import prop.PropertyChecks


class Tests extends FreeSpec with PropertyChecks with Matchers
{
	"tests" in
	{
		interpret( """ [= 1 1] """ ) shouldBe true
		interpret( """ [= 1 2] """ ) shouldBe false
		interpret( """ [if [< 1 2] "yes" "no"] """ ) shouldBe "yes"
		interpret( """ [[lambda [x y] [+ x y]] 3 4] """ ) shouldBe 7
		interpret( """ [[lambda x x] 3] """ ) shouldBe List( 3 )
		interpret( """ [define f [lambda [x y] [+ [* x x] [* y y]]]] [define a 5] [f a 4] """ ) shouldBe 41
 		interpret( """ [define x 5] [set! x 6] [+ x 1] """ ) shouldBe 7
		a [RuntimeException] should be thrownBy {interpret( """ (= 1 1] """ )}
		interpret( """ [if #f "yes" "no"] """ ) shouldBe "no"
		interpret( """ [if #t "yes" "no"] """ ) shouldBe "yes"
	}
}
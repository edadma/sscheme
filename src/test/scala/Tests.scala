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
	}
}
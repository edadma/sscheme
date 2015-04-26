package ca.hyperreal.sscheme


object BasicPrimitives extends Primitives
{
	val list = Seq(
		new Primitive( "eq?" )( {case SList(first: AnyRef, second: AnyRef) => (first eq second)} ),
		new Primitive( "car" )( {case SList(list: SList) => list.head} ),
		new Primitive( "cdr" )( {case SList(list: SList) => list.tail} ),
		new Primitive( "cons" )( {case SList(first, second) => SPair( first, second )} ),
		new Primitive( "display" )( {case SList(obj) => println(obj)} )
		)
}
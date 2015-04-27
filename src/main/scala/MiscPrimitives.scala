package ca.hyperreal.sscheme


object MiscPrimitives extends Primitives
{
	val list = Seq(
		new Primitive( "display" )( {case SList(obj) => println(obj)} ),
		new Primitive( "begin" )( {case exps: SList => exps.last} )
		)
}
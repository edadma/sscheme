package ca.hyperreal.sscheme


object ListPrimitives extends Primitives
{
	val list = Seq(
		new Primitive( "car" )( {case SList(list: SList) => list.head} ),
		new Primitive( "cdr" )( {case SList(list: SList) => list.tail} ),
		new Primitive( "cons" )( {case SList(first, second) => SPair( first, second )} ),
		new Primitive( "cddr" )( {case SList(list: SList) => list.tailSList.tail} ),
		new Primitive( "set-car!" )( {case SList(pair: SPair, obj) => pair.head = obj} ),
		new Primitive( "set-cdr!" )( {case SList(pair: SPair, obj) => pair.tail = obj} )
		)
}
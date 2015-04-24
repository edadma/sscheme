package ca.hyperreal.sscheme

import ca.hyperreal.lia.Math


object NumericPrimitives extends Primitives
{
	val list = Seq(
		new Primitive( "+" )(
			{
				case Nil => 0
				case list: List[Number] => list.reduce( Math('+, _, _).asInstanceOf[Number] )
			} ),
		new Primitive( "*" )(
			{
				case Nil => 1
				case list: List[Number] => list.reduce( Math('*, _, _).asInstanceOf[Number] )
			} ),
		new Primitive( "-" )(
			{
				case List( n ) => Math( '-, n )
				case list: List[Number] => list.reduce( Math('-, _, _).asInstanceOf[Number] )
			} ),
		new Primitive( "/" )(
			{
				case List( n ) => Math( '/, 1, n )
				case list: List[Number] => list.reduce( Math('/, _, _).asInstanceOf[Number] )
			} ),
		new Primitive( "quotient" )( {case List(first: Int, second: Int) => Math( Symbol("\\"), first, second )} ),
		new Primitive( "sqrt" )( {case List( n ) => ca.hyperreal.lia.Math.sqrtFunction( n )} ),
		new Primitive( "<" )( {case List(first: Number, second: Number) => Math( '<, first, second )} ),
		new Primitive( ">" )( {case List(first: Number, second: Number) => Math( '>, first, second )} ),
		new Primitive( "<=" )( {case List(first: Number, second: Number) => Math( '<=, first, second )} ),
		new Primitive( ">=" )( {case List(first: Number, second: Number) => Math( '>=, first, second )} ),
		new Primitive( "=" )( {case List(first: Number, second: Number) => Math( '==, first, second )} )
		)
}
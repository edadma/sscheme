package ca.hyperreal.sscheme

import ca.hyperreal.lia.Math


object NumericPrimitives extends Primitives
{
	val list = Seq(
		new Primitive( "+" )(
			{
				case SNil => 0
				case list: SList => list.reduce( Math('+, _, _) )
			} ),
		new Primitive( "*" )(
			{
				case SNil => 1
				case list: SList => list.reduce( Math('*, _, _) )
			} ),
		new Primitive( "-" )(
			{
				case SList( n ) => Math( '-, n )
				case list: SList => list.reduce( Math('-, _, _) )
			} ),
		new Primitive( "/" )(
			{
				case SList( n ) => Math( '/, 1, n )
				case list: SList => list.reduce( Math('/, _, _) )
			} ),
		new Primitive( "quotient" )( {case SList(first, second) => Math( Symbol("\\"), first, second )} ),
		new Primitive( "sqrt" )( {case SList( n ) => ca.hyperreal.lia.Math.sqrtFunction( n )} ),
		new Primitive( "<" )( {case SList(first, second) => Math( '<, first, second )} ),
		new Primitive( ">" )( {case SList(first, second) => Math( '>, first, second )} ),
		new Primitive( "<=" )( {case SList(first, second) => Math( '<=, first, second )} ),
		new Primitive( ">=" )( {case SList(first, second) => Math( '>=, first, second )} ),
		new Primitive( "=" )( {case SList(first, second) => Math( '==, first, second )} )
		)
}
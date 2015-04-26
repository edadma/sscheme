package ca.hyperreal.sscheme


object TypePrimitives extends Primitives
{
	val list = Seq(
		new Primitive( "integer?" )(
			{
				case SList(_: Int | _: BigInt) => true
				case SList( _ ) => false
			} ),
		new Primitive( "port?" )( {case SList( a ) => a.isInstanceOf[IOPrimitives.Port] || a.isInstanceOf[java.io.PrintStream]} ),
		new Primitive( "symbol?" )( {case SList( a ) => a.isInstanceOf[Symbol]} ),
		new Primitive( "number?" )( {case SList( a ) => a.isInstanceOf[Number]} ),
		new Primitive( "char?" )( {case SList( a ) => a.isInstanceOf[Char]} ),
		new Primitive( "string?" )( {case SList( a ) => a.isInstanceOf[String]} ),
		new Primitive( "vector?" )( {case SList( a ) => a.isInstanceOf[Array[Any]]} ),
		new Primitive( "procedure?" )( {case SList( a ) => a.isInstanceOf[Procedure]} )
	)
}
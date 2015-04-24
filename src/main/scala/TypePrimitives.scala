package ca.hyperreal.sscheme


object TypePrimitives extends Primitives
{
	val list = Seq(
		new Primitive( "integer?" )(
			{
				case List(_: Int | _: BigInt) => true
				case List( _ ) => false
			} ),
		new Primitive( "port?" )( {case List( a ) => a.isInstanceOf[IOPrimitives.Port] || a.isInstanceOf[java.io.PrintStream]} ),
		new Primitive( "symbol?" )( {case List( a ) => a.isInstanceOf[Symbol]} ),
		new Primitive( "number?" )( {case List( a ) => a.isInstanceOf[Number]} ),
		new Primitive( "char?" )( {case List( a ) => a.isInstanceOf[Char]} ),
		new Primitive( "string?" )( {case List( a ) => a.isInstanceOf[String]} ),
		new Primitive( "vector?" )( {case List( a ) => a.isInstanceOf[Array[Any]]} ),
		new Primitive( "procedure?" )( {case List( a ) => a.isInstanceOf[Procedure]} )
	)
}
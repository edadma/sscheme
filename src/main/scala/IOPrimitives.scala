package ca.hyperreal.sscheme

import java.io.{Reader,StringReader, PrintStream}


object IOPrimitives extends Primitives
{
	private case object eof
	
	private var inputPort = new Port( Console.in )
	
	class Port( r: Reader )
	{
		def read =
			r.read match
			{
				case -1 => eof
				case c => c.toChar
			}
			
		def ready = r.ready
	}

	val list = Seq(
		new Primitive( "eof-object?" )( {case List( a: AnyRef ) => a eq eof} ),
		new Primitive( "string-port" )( {case List( s: String ) => new Port( new StringReader(s) )} ),
		new Primitive( "read-char" )(
			{
				case Nil => inputPort.read
				case List( p: Port ) => p.read
			} ),
		new Primitive( "write-char" )(
			{
				case List( c: Char ) => Console.out.print( c )
				case List( c: Char, p: PrintStream ) => p.print( c )
			} )
		)
}
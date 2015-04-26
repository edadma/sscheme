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
		new Primitive( "eof-object?" )( {case SList( a: AnyRef ) => a eq eof} ),
		new Primitive( "string-port" )( {case SList( s: String ) => new Port( new StringReader(s) )} ),
		new Primitive( "read-char" )(
			{
				case SNil => inputPort.read
				case SList( p: Port ) => p.read
			} ),
		new Primitive( "write-char" )(
			{
				case SList( c: Char ) => Console.out.print( c )
				case SList( c: Char, p: PrintStream ) => p.print( c )
			} )
		)
}
package ca.hyperreal.sscheme

import java.io.{FileReader, PrintWriter}

import jline.console.ConsoleReader


object Main extends App
{
	if (!args.isEmpty)
	{
	val script = new FileReader( args(0) )
	
		interpret( Parser.parse(script) )( standardEnvironment add ('args -> SList.fromScalaSeq(args.toList.tail)) ) match
		{
			case () =>
			case result => println( result )
		}
		
		script.close
		sys.exit
	}
	
	System.getProperties.setProperty( "jline.shutdownhook", "true" )

	val reader = new ConsoleReader
	val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
	var line: String = null
	var count = 1
	implicit val env = standardEnvironment

	reader.setBellEnabled( false )
	reader.setPrompt( "SScheme> " )

	out.println( "Welcome to SScheme version " + VERSION )
	out.println( "Type in expressions to have them evaluated." )
	out.println( "Type :help for more information." )
	out.println
	
	while ({line = reader.readLine; line != null})
	{
		line.trim match
		{
			case ":help" =>
				out.println( ":help                      print this summary" )
				out.println( ":quit                      exit the REPL" )
				out.println
			case ":quit" =>
				sys.exit
			case "" =>
				out.println
			case _ =>
				try
				{
					interpret( line, env ) match
					{
						case () =>
						case res =>
						val name = "res" + count

							if (res == null)
								out.println( name + " = null" )
							else
								out.println( name + ": " + res.getClass.getName + " = " + res )
								
							env add (Symbol(name) -> res)
							count += 1
					}
					
					out.println
				}
				catch
				{
					case e: Exception =>
//					e.printStackTrace( out )
						out.println( e )
						out.println
				}
		}
	}
}
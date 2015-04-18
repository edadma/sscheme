package ca.hyperreal.sscheme

import java.io.{Reader, StringReader}

import collection.mutable.ListBuffer

import ca.hyperreal.rtcep._


object Parser
{
	class SchemeAtomLexeme( tok: Any ) extends Lexeme
	{
		private val atomchar = ('0' to '9').toSet ++ ('a' to 'z') ++ ('A' to 'Z') ++ "?!.+-*/<=>:$%^&_~@"
		
		def token( s: Stream[Chr] ) =
			if (s.head.ch != '@' && atomchar(s.head.ch))
				Some( consume(tok, s, c => atomchar(c)) )
			else
				None
	}

// 	class SchemeSpecialLexeme( toks: (Any, String)* ) extends Lexeme
// 	{
// 		private val atomchar = ('0' to '9').toSet ++ ('a' to 'z') ++ ('A' to 'Z') ++ "?!.+-*/<=>:$%^&_~@"
// 		
// 		def token( s: Stream[Chr] ) =
// 			if (s.head.ch != '@' && atomchar(s.head.ch))
// 				Some( consume(tok, s, c => atomchar(c)) )
// 			else
// 				None
// 	}

	val lexer =
		new Lexer
		{
			add( new StringLexeme('string, '"') )
			add( new FloatingLexeme('float) )
			add( new IntegerLexeme('integer) )
			ignore( new LineCommentLexeme(";") )
			ignore( new BlockCommentLexeme("/*", "*/") )
			add( new SchemeAtomLexeme('atom) )
			add( new SymbolLexeme( 'atom ) {add( "(", ")", "[", "]", "'", "#(" )} )
			ignore( WhitespaceLexeme )
			add( EOFLexeme )
		}

	def scan( r: Reader ) = lexer.scan( r, 4 )

	def scan( s: String ): Stream[Token] = scan( new StringReader(s) )
	
	def parse( t: Stream[Token] ): List[List[Any]] =
	{
	val buf = new ListBuffer[List[Any]]
	
		def build( s: Stream[Token] ): List[List[Any]] =
			if (s.head.end)
				if (buf.isEmpty)
					s.head.pos.error( "unexpected end of input" )
				else
					buf.toList
			else
			{
			val (expr, rest) = parseList( s )
			
				buf += expr
				build( rest )
			}

		build( t )
	}
	
	def parse( r: Reader ): List[List[Any]] = parse( scan(r) )
	
	def parse( s: String ): List[List[Any]] = parse( scan(s) )
	
	def parseExpression( s: Stream[Token] ): (Any, Stream[Token]) =
		if (s.head.kind == '(' || s.head.kind == '[' || s.head.kind == '\'')
			parseList( s )
		else
			((s.head.kind match
			{
				case 'atom => Symbol( s.head.s )
				case 'integer => s.head.s.toInt
				case 'float => s.head.s.toDouble
				case 'string => s.head.s
			}), s.tail)
	
	def parseList( t: Stream[Token] ): (List[Any], Stream[Token]) =
		if (t.head.kind == '(' || t.head.kind == '[')
		{
		val buf = new ListBuffer[Any]
		
			def build( s: Stream[Token] ): (List[Any], Stream[Token]) =
				if (s.head.end)
					s.head.pos.error( "unclosed list" )
				else if (s.head.kind == ')' || s.head.kind == ']')
					(buf.toList, s.tail)
				else
				{
				val (expr, rest) = parseExpression( s )
				
					buf += expr
					build( rest )
				}
				
			build( t.tail )
		}
		else if (t.head.kind == '\'')
		{
		val (expr, rest) = parseExpression( t.tail )
		
			(List('quote, expr), rest)
		}
		else
			t.head.pos.error( """expected "(" or "[" or "'"""" )
}
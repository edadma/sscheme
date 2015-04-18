package ca.hyperreal

import collection.mutable.ListBuffer

import ca.hyperreal.lia.Math


package object sscheme
{
	type Environment = Map[Symbol, Any]
	
	abstract class Form
	{
		def apply( args: List[Any] )( implicit env: Environment ): (Any, Environment)
	}
	
	class Procedure( procedure: List[Any] => Any ) extends Form
	{
		def apply( args: List[Any] )( implicit env: Environment ) = (procedure( args map (aeval) ), env)
	}
	
	class Primitive( primitive: String )( procedure: PartialFunction[List[Any], Any] ) extends Procedure( procedure )
	{
		override def apply( args: List[Any] )( implicit env: Environment ) =
			if (procedure.isDefinedAt( args ))
				super.apply( args )
			else
				error( s"invalid arguments for '$primitive': $args" )
	}
	
	class Syntax( form: List[Any] => Any ) extends Form
	{
		def apply( args: List[Any] )( implicit env: Environment ) = (form( args ), env)
	}
	
	class Lambda( formals: Either[List[Symbol], Symbol], body: List[Any], env: Environment ) extends
		Procedure(
			{args =>
				formals match
				{
					case Left( f ) =>
						val al = args.length
						val pl = f.length
					
						if (al < pl)
							error( "too few arguments for lambda: " + args )
						else if (al > pl)
							error( "too many arguments for lambda: " + args )
						else
							interpret( body )( env ++ (f zip args) )
					case Right( f ) =>
						interpret( body )( env + (f -> args) )
				}
			}
		)
	
	private val GLOBAL =
		Map[Symbol, Any](
			'quote -> new Syntax(
				list =>
					if (list.length == 1)
						list.head
					else
						error( "wrong number of arguments for 'quote': " + list )
				),
			'+ -> new Primitive( "+" )(
				{
					case Nil => 0
					case list: List[Number] => list.reduce( Math('+, _, _).asInstanceOf[Number] )
				} ),
			'* -> new Primitive( "*" )(
				{
					case Nil => 1
					case list: List[Number] => list.reduce( Math('*, _, _).asInstanceOf[Number] )
				} ),
			'- -> new Primitive( "-" )( {case List(first: Int, second: Int) => first - second} ),
			'/ -> new Primitive( "/" )( {case List(first: Int, second: Int) => first / second} ),
			'< -> new Primitive( "<" )( {	case List(first: Int, second: Int) => first < second} ),
			'> -> new Primitive( ">" )( {	case List(first: Int, second: Int) => first > second} ),
			'<= -> new Primitive( "<=" )( {	case List(first: Int, second: Int) => first <= second} ),
			'>= -> new Primitive( ">=" )( {	case List(first: Int, second: Int) => first >= second} ),
			'= -> new Primitive( "=" )( {	case List(first: Number, second: Number) => Math( '==, first, second )} ),
			Symbol("eq?") -> new Primitive( "eq?" )( {case List(first: AnyRef, second: AnyRef) => (first eq second)} ),
			'car -> new Primitive( "car" )( {case List(list: List[Any]) => list.head} ),
			'cdr -> new Primitive( "car" )( {case List(list: List[Any]) => list.tail} ),
			'cons -> new Primitive( "cons" )( {case List(first, second: List[Any]) => first :: second} ),
			'define ->
				new Form
				{
					def apply( args: List[Any] )( implicit env: Environment ) =
						args match
						{
							case List( sym: Symbol, exp ) => (Nil, env + (sym -> aeval(exp)))
						}
				},
			'apply ->
				new Form
				{
					def apply( args: List[Any] )( implicit env: Environment ) =
						args map (eval) match
						{
							case (function: Form) :: obj :: rest if rest != Nil => 
								rest.last match
								{
									case l: List[Any] =>
										val buf = ListBuffer[Any]( obj )
										
										buf ++= rest.init
										buf ++= l
										function( buf.toList )
									case a => error( "expected list as last argument of 'apply': " + a )
								}
							case List(function: Form, list: List[Any]) => function( list )
							case _ => error( "invalid arguments for 'apply': " + args )
						}
				},
			'lambda ->
				new Form
				{
					def apply( args: List[Any] )( implicit env: Environment ) =
						args match
						{
							case (formals: List[Symbol]) :: body => (new Lambda( Left(formals), body, env ), env)
							case (formals: Symbol) :: body => (new Lambda( Right(formals), body, env ), env)
							case _ => error( "invalid arguments for 'lambda': " + args )
						}
				},
			'if ->
				new Form
				{
					def apply( args: List[Any] )( implicit env: Environment ) =
						args match
						{
							case List(test, consequent) =>
								if (beval( test ))
									eval( consequent )
								else
									(Nil, env)
							case List(test, consequent, alternative) =>
								if (beval( test ))
									eval( consequent )
								else
									eval( alternative )
							case a => error( "invalid arguments for 'if': " + a )							
						}
				}
		)

	def interpret( program: List[Any] )( implicit env: Environment = GLOBAL ): Any =
		if (program != Nil)
		{
		val (result, newenv) = eval( program.head )
		
			if (program.tail == Nil)
				result
			else
				interpret( program.tail )( newenv )
		}
		else
			Nil

	def interpret( program: String ): Any = interpret( Parser.parse(program) )
	
	def eval( expr: Any )( implicit env: Environment ): (Any, Environment) =
		expr match
		{
			case _: Int | _: Double | _: String | Nil => (expr, env)
			case s: Symbol => (env.getOrElse( s, error("unbound symbol: " + s.name) ), env)
			case head :: tail =>
				aeval( head ) match
				{
					case f: Form => f( tail )
					case h => error( "head of list not applicable: " + h )
				}
		}
	
	def beval( expr: Any )( implicit env: Environment ): Boolean =
		aeval( expr ) match
		{
			case b: Boolean => b
			case a => error( "expected boolean: " + a )
		}
	
	def aeval( expr: Any )( implicit env: Environment ) = eval( expr )._1
}
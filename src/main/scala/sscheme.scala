package ca.hyperreal

import collection.mutable.{HashMap, ListBuffer}

import ca.hyperreal.lia.Math


package object sscheme
{
	type SchemePair = (Any, Any)
	
	private [sscheme] class Environment( outer: Environment ) extends HashMap[Symbol, Holder]
	{
		def add( kvs: (Symbol, Any)* ): Environment =
		{
			for ((k, v) <- kvs)
				put( k, new Holder(v) )
				
			this
		}

		def find( variable: Symbol ): Option[Holder] =
			get( variable ) match
			{
				case None =>
					if (outer eq null)
						None
					else
						outer find variable
				case sh => sh
			}
	}
	
	private [sscheme] class Holder( var obj: Any )
	{
		override def toString = String.valueOf( obj )
	}
	
	abstract class Form
	{
		def apply( args: List[Any] )( implicit env: Environment ): Any
	}
	
	class Procedure( procedure: List[Any] => Any ) extends Form
	{
		def apply( args: List[Any] )( implicit env: Environment ) = procedure( args map eval )
	}
	
	class Primitive( primitive: String )( procedure: PartialFunction[List[Any], Any] ) extends Form
	{
		def apply( args: List[Any] )( implicit env: Environment ) =
		{
		val evaled = args map eval
		
			if (procedure.isDefinedAt( evaled ))
				procedure( evaled )
			else
				sys.error( s"invalid arguments for '$primitive': $evaled" )
		}
	}
	
	class Syntax( form: List[Any] => Any ) extends Form
	{
		def apply( args: List[Any] )( implicit env: Environment ) = form( args )
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
							sys.error( s"too few arguments for lambda: $args" )
						else if (al > pl)
							sys.error( s"too many arguments for lambda: $args" )
						else
							interpret( body )( new Environment(env) add (f.zip(args): _*) )
					case Right( f ) =>
						interpret( body )( new Environment(env) add (f -> args) )
				}
			}
		)
	
	private val GLOBAL =
		new Environment( null ).add(
			'quote -> new Syntax(
				list =>
					if (list.length == 1)
						list.head
					else
						sys.error( "wrong number of arguments for 'quote': " + list )
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
			'sqrt -> new Primitive( "sqrt" )( {case List(n) => ca.hyperreal.lia.Math.sqrtFunction( n )} ),
			'< -> new Primitive( "<" )( {	case List(first: Int, second: Int) => first < second} ),
			'> -> new Primitive( ">" )( {	case List(first: Int, second: Int) => first > second} ),
			'<= -> new Primitive( "<=" )( {	case List(first: Int, second: Int) => first <= second} ),
			'>= -> new Primitive( ">=" )( {	case List(first: Int, second: Int) => first >= second} ),
			'= -> new Primitive( "=" )( {	case List(first: Number, second: Number) => Math( '==, first, second )} ),
			Symbol("eq?") -> new Primitive( "eq?" )( {case List(first: AnyRef, second: AnyRef) => (first eq second)} ),
			'car -> new Primitive( "car" )( {case List(list: List[Any]) => list.head} ),
			'cdr -> new Primitive( "car" )( {case List(list: List[Any]) => list.tail} ),
			'cons -> new Primitive( "cons" )( {case List(first, second: List[Any]) => first :: second} ),
			'display -> new Primitive( "display" )( {case List(obj) => println(obj)} ),
			'define ->
				new Form
				{
					def apply( args: List[Any] )( implicit env: Environment ) =
						args match
						{
							case List( sym: Symbol, exp ) =>
								env += (sym -> new Holder(eval(exp)))
								Nil
						}
				},
			Symbol("set!") ->
				new Form
				{
					def apply( args: List[Any] )( implicit env: Environment ) =
						args match
						{
							case List( sym: Symbol, exp ) =>
								env.find( sym ) match
								{
									case None => sys.error( s"variable ${sym.name} was not previously bound: (set! ${sym.name} $exp)" )
									case Some( h: Holder ) => h.obj = eval( exp )
								}
								
								Nil
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
									case a => sys.error( "expected list as last argument of 'apply': " + a )
								}
							case List(function: Form, list: List[Any]) => function( list )
							case _ => sys.error( "invalid arguments for 'apply': " + args )
						}
				},
			'lambda ->
				new Form
				{
					def apply( args: List[Any] )( implicit env: Environment ) =
						args match
						{
							case (formals: List[Symbol]) :: body => new Lambda( Left(formals), body, env )
							case (formals: Symbol) :: body => new Lambda( Right(formals), body, env )
							case _ => sys.error( "invalid arguments for 'lambda': " + args )
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
							case a => sys.error( "invalid arguments for 'if': " + a )							
						}
				},
			'or ->
				new Form
				{
					def apply( args: List[Any] )( implicit env: Environment ) = args exists beval
				},
			'and ->
				new Form
				{
					def apply( args: List[Any] )( implicit env: Environment ) = args forall beval
				},
			'let ->
				new Form
				{
					def apply( args: List[Any] )( implicit env: Environment ) =
						args match
						{
							case (bindings: List[Any]) :: body /*if properList( bindings )*/ =>
								interpret( body )( new Environment(env) add ((bindings map ({case List(k: Symbol, v) => (k, eval(v))})): _*) )
							case a => sys.error( "invalid arguments for 'let': " + a )							
						}
				}

		)

		
	interpret( """
		(define null? (lambda (x) (eq? x '())))
		
		(define boolean? (lambda (x) (or (eq? x #t) (eq? x #f))))
		
		(define not (lambda (x) (if x #f #t)))
		
		(define list (lambda x x))
		""" )
	
	def interpret( program: List[Any] )( implicit env: Environment = GLOBAL ): Any =
		if (program != Nil)
		{
		val result = eval( program.head )
		
			if (program.tail == Nil)
				result
			else
				interpret( program.tail )
		}
		else
			Nil

	def interpret( program: String ): Any = interpret( Parser.parse(program) )
	
	def eval( expr: Any )( implicit env: Environment ): Any =
		expr match
		{
			case _: Int | _: Double | _: String | false | true | Nil => expr
			case s: Symbol => env.find( s ).getOrElse( sys.error("unbound symbol: " + s.name) ).obj
			case head :: tail =>
				eval( head ) match
				{
					case f: Form => f( tail )
					case h => sys.error( "head of list not applicable: " + h )
				}
		}
	
	def beval( expr: Any )( implicit env: Environment ): Boolean =
		eval( expr ) match
		{
			case b: Boolean => b
			case a => sys.error( "expected boolean: " + a )
		}
	
	def properList( pair: SchemePair ): Boolean =
		pair match
		{
			case (_, Nil) => true
			case (_, tail: SchemePair) => properList( tail )
			case _ => false
		}
	
	def toScalaList( list: SchemePair ): (List[Any], Any) =
	{
	val buf = new ListBuffer[Any]
	
		(buf.toList, null)
	}
}
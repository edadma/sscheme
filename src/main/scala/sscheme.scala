package ca.hyperreal

import collection.mutable.{HashMap, ListBuffer}

import ca.hyperreal.lia.Math


package object sscheme
{
	var trace = false
	
	type SchemePair = (Any, Any)
	
	private [sscheme] class Environment( outer: Environment ) extends HashMap[Symbol, Holder]
	{
		def add( key: Symbol, value: Any ) =
		{
			put( key, new Holder(value) )
			this
		}
		
		def add( kvs: (Symbol, Any)* ): Environment =
		{
			for ((k, v) <- kvs)
				add( k, v )
				
			this
		}
		
		def add( ps: Primitives ): Environment =
		{
			for (p <- ps.list)
				add( Symbol(p.primitive), p )
				
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
	
	abstract class Primitives
	{
		val list: Seq[Primitive]
	}
	
	abstract class Form
	{
		def apply( args: List[Any] )( implicit env: Environment ): Any
	}
	
	class Procedure( procedure: List[Any] => Any ) extends Form
	{
		def apply( args: List[Any] )( implicit env: Environment ) = procedure( args map eval )
	}
	
	class Primitive( val primitive: String )( procedure: PartialFunction[List[Any], Any] ) extends Form
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
							case (name: Symbol) :: (bindings: List[Any]) :: body /*if properList( bindings )*/ =>
								val newenv = new Environment( env )
								val lambda = new Lambda( Left(bindings map ({case List(k: Symbol, _) => k})), body, newenv )
		
								newenv(name) = new Holder( lambda )
								lambda( bindings map ({case List(_, v) => v}) )( newenv )
							case a => sys.error( "invalid arguments for 'let': " + a )							
						}
				},
			'cond ->
				new Form
				{
					def apply( args: List[Any] )( implicit env: Environment ) =
					{
						def evaluate( clauses: List[Any] ): Any =
							clauses.head match
							{
								case Nil => Nil
								case List( test ) =>
									eval( test ) match
									{
										case false => evaluate( clauses.tail )
										case v => v
									}
								case test :: '=> :: exp =>
									val res = eval( test )
									
										if (res != false)
											eval( exp ).asInstanceOf[Procedure]( List(res) )
										else
											evaluate( clauses.tail )
								case 'else :: body if clauses.tail.isEmpty => interpret( body )
								case test :: body =>
									if (beval( test ))
										interpret( body )
									else
										evaluate( clauses.tail )
								case a => sys.error( "invalid arguments for 'test': " + a )							
							}
							
						evaluate( args )
					}
				}
		).
		add( NumericPrimitives ).
		add( IOPrimitives )
	
	interpret( """
		(define null? (lambda (x) (eq? x '())))
		
		(define boolean? (lambda (x) (or (eq? x #t) (eq? x #f))))
		
		(define not (lambda (x) (if x #f #t)))
		
		(define list (lambda x x))
		
		(define abs (lambda (x) (if (< x 0) (- x) x)))
		
		(define length
			(lambda (ls)
				(let loop ((ls ls) (n 0))
				(if (null? ls)
					n
					(loop (cdr ls) (+ n 1))))))
		
		(define list-tail
			(lambda (ls n)
				(if (= n 0)
					ls
					(list-tail (cdr ls) (- n 1)))))
		""", GLOBAL )

	def standardEnvironment = new Environment( GLOBAL )
	
	def interpret( program: List[Any] )( implicit env: Environment = standardEnvironment ): Any =
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

	def interpret( program: String ): Any = interpret( program, standardEnvironment )
	
	def interpret( program: String, env: Environment ): Any = interpret( Parser.parse(program) )( env )

	def environment( program: String ): Environment =
	{
	val env = new Environment( standardEnvironment )
	
		interpret( program, env )
		env
	}
	
	def eval( expr: Any )( implicit env: Environment ): Any =
	{
		if (trace)
			println( ">>>> " + expr )
		
		expr match
		{
			case _: Int | _: Double | _: String | false | true | Nil => expr
			case s: Symbol => env.find( s ).getOrElse( sys.error("unbound symbol: " + s.name) ).obj
			case head :: tail =>
				eval( head ) match
				{
					case f: Form => f( tail )
					case h => sys.error( "head of list not applicable: " + (h :: tail) )
				}
		}
	}
	
	def beval( expr: Any )( implicit env: Environment ): Boolean =
		eval( expr ) match
		{
			case b: Boolean => b
			case _ => true
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
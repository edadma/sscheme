package ca.hyperreal

import collection.mutable.HashMap

import ca.hyperreal.lia.Math


package object sscheme
{
	var trace = false
	val VERSION =
		{
			val p = getClass.getPackage
			val name = p.getImplementationTitle

			p.getImplementationVersion
		}
	
	class Environment( outer: Environment ) extends HashMap[Symbol, Holder]
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
	
	class Holder( var obj: Any )
	{
		override def toString = String.valueOf( obj )
	}
	
	abstract class Primitives
	{
		val list: Seq[Primitive]
	}
	
	abstract class Form
	{
		def apply( args: SList )( implicit env: Environment ): Any
	}
	
	class Procedure( procedure: SList => Any ) extends Form
	{
		def apply( args: SList )( implicit env: Environment ) = procedure( args map eval )
	}
	
	class Primitive( val primitive: String )( procedure: PartialFunction[SList, Any] ) extends Form
	{
		def apply( args: SList )( implicit env: Environment ) =
		{
		val evaled = args map eval
		
			if (procedure.isDefinedAt( evaled ))
				procedure( evaled )
			else
				sys.error( s"invalid arguments for '$primitive': $evaled" )
		}
	}
	
	class Syntax( form: SList => Any ) extends Form
	{
		def apply( args: SList )( implicit env: Environment ) = form( args )
	}
	
	class Lambda( formals: Either[List[Symbol], Symbol], body: SList, env: Environment ) extends
		Procedure(
			{args =>
				formals match
				{
					case Left( f ) =>
						val argsList = args.toProperList
						val al = argsList.length
						val pl = f.length
					
						if (al < pl)
							sys.error( s"too few arguments for lambda: $args" )
						else if (al > pl)
							sys.error( s"too many arguments for lambda: $args" )
						else
							interpret( body )( new Environment(env) add (f.zip(argsList): _*) )
					case Right( f ) =>
						interpret( body )( new Environment(env) add (f -> args) )
				}
			}
		)
	
	val GLOBAL =
		new Environment( null ).add(
			'quote -> new Syntax(
				_ match
				{
					case SList( arg ) => arg
					case args => sys.error( "invalid arguments for 'quote': " + args )
				} ),
			'define ->
				new Form
				{
					def apply( args: SList )( implicit env: Environment ) =
						args match
						{
							case SList( sym: Symbol, exp ) =>
								env += (sym -> new Holder(eval(exp)))
								()
						}
				},
			Symbol("set!") ->
				new Form
				{
					def apply( args: SList )( implicit env: Environment ) =
						args match
						{
							case SList( sym: Symbol, exp ) =>
								env.find( sym ) match
								{
									case None => sys.error( s"variable ${sym.name} was not previously bound: (set! ${sym.name} $exp)" )
									case Some( h: Holder ) =>
										h.obj = eval( exp )
										()
								}
						}
				},
			'apply ->
				new Form
				{
					def apply( args: SList )( implicit env: Environment ) =
						args map (eval) match
						{
							case SList( function: Form, list: SList ) if list.isProperList => function( list )
							case SPair( (function: Form), args: SList ) if args.isProperList => 
								val argsList = args.toProperList
								
								argsList.last match
								{
									case p: SList if p.isProperList => function( SList.fromScalaSeq(argsList.init ++ p.toProperList) )
									case a => sys.error( "expected proper list as last argument of 'apply': " + a )
								}
							case _ => sys.error( "invalid arguments for 'apply': " + args )
						}
				},
			'lambda ->
				new Form
				{
					def apply( args: SList )( implicit env: Environment ) =
					{
						args match
						{
							case SPair( formals: SList, body: SList ) if formals.isProperList && body.isProperList =>
								new Lambda( Left(formals.toProperList.asInstanceOf[List[Symbol]]), body, env )
							case SPair( (formal: Symbol), body: SList ) if body.isProperList => new Lambda( Right(formal), body, env )
							case _ => sys.error( "invalid arguments for 'lambda': " + args )
						}
					}
				},
			'if ->
				new Form
				{
					def apply( args: SList )( implicit env: Environment ) =
						args match
						{
							case SList(test, consequent) =>
								if (beval( test ))
									eval( consequent )
								else
									(Nil, env)
							case SList(test, consequent, alternative) =>
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
					def apply( args: SList )( implicit env: Environment ) = args exists beval
				},
			'and ->
				new Form
				{
					def apply( args: SList )( implicit env: Environment ) = args forall beval
				},
			'let ->
				new Form
				{
					def apply( args: SList )( implicit env: Environment ) =
						args match
						{
							case SPair( bindings: SList, body: SList ) if bindings.isProperList =>
								interpret( body )( new Environment(env) add
									((bindings map ({case SList(k: Symbol, v) => (k, eval(v))})).toProperList.asInstanceOf[List[(Symbol, Any)]]: _*) )
							case SPair( (name: Symbol), SPair((bindings: SList), body: SList) ) if bindings.isProperList =>
								val newenv = new Environment( env )
								val lambda = new Lambda( Left(bindings.map( {case SList(k: Symbol, _) => k} ).
									toProperList.asInstanceOf[List[Symbol]]), body, newenv )
		
								newenv(name) = new Holder( lambda )
								lambda( bindings map ({case SList(_, v) => v}) )( newenv )
							case a => sys.error( "invalid arguments for 'let': " + a )							
						}
				},
			'cond ->
				new Form
				{
					def apply( args: SList )( implicit env: Environment ) =
					{
						def evaluate( clauses: SList ): Any =
							clauses.head match
							{
								case SNil => ()
								case SList( test ) =>
									eval( test ) match
									{
										case false => evaluate( clauses.tailSList )
										case v => v
									}
								case SPair( test, SPair('=>, exp) ) =>
									val res = eval( test )
									
										if (res != false)
											eval( exp ).asInstanceOf[Procedure]( SList(res) )
										else
											evaluate( clauses.tailSList )
								case SPair( 'else, body: SList ) if clauses.tailSList.isEmpty => interpret( body )
								case SPair( test, body: SList ) =>
									if (beval( test ))
										interpret( body )
									else
										evaluate( clauses.tailSList )
								case a => sys.error( "invalid arguments for 'test': " + a )							
							}
							
						evaluate( args )
					}
				}
		).
		add( ListPrimitives ).
		add( MiscPrimitives ).
		add( NumericPrimitives ).
		add( IOPrimitives ).
		add( TypePrimitives )
	
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
		
		(define list?
			(lambda (x)
				(let race ((h x) (t x))
				(if (pair? h)
					(let ((h (cdr h)))
						(if (pair? h)
							(and (not (eq? h t))
								(race (cdr h) (cdr t)))
							(null? h)))
					(null? h)))))
		""", GLOBAL )

	def standardEnvironment = new Environment( GLOBAL )
	
	def interpret( program: SList )( implicit env: Environment = standardEnvironment ): Any =
		program match
		{
			case SNil => ()
			case SPair( head, tail: SList ) => 
				val result = eval( head )
				
				if (tail == SNil)
					result
				else
					interpret( tail )
			case _ => sys.error( "can't interpret improper list" )
		}

	def interpret( program: String ): Any = interpret( program, standardEnvironment )

	def interpret( program: String, vars: (Symbol, Any)* ): Any = interpret( program, standardEnvironment add (vars: _*) )
	
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
			case SPair( head, tail: SList ) =>
				eval( head ) match
				{
					case f: Form => f( tail )
					case h => sys.error( "head of list not applicable: " + SPair(h, tail) )
				}
			case _ => sys.error( "improper list can't be evaluated: " + expr )
		}
	}
	
	def beval( expr: Any )( implicit env: Environment ): Boolean =
		eval( expr ) match
		{
			case b: Boolean => b
			case _ => true
		}
}
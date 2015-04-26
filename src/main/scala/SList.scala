package ca.hyperreal.sscheme

import util.Either
import collection.mutable.ListBuffer


abstract class SList
	{
		def head: Any

		def tail: Any
		
		lazy val tailSList = tail.asInstanceOf[SList]
		
		def isEmpty: Boolean
		
		def map( f: Any => Any ): SList =
			this match
			{
				case SNil => SNil
				case SPair( head, tail: SList ) => SPair( f(head), tail map f )
				case SPair( left, right ) => sys.error( s"SList.map: improper list: ($left $right)" )
			}
			
		def reduce( f: (Any, Any) => Any ) =
		{
			def _reduce( res: Any, tail: Any ): Any =
				tail match
				{
					case SNil => res
					case SPair( a, tail: SList ) => _reduce( f(res, a), tail )
					case _ => sys.error( "SList.reduce: can't reduce an improper list" )
				}

			this match
			{
				case SNil => sys.error( "SList.reduce: can't reduce an empty list" )
				case SPair( a, tail ) => _reduce( a, tail )
			}
		}
		
		def exists( p: Any => Boolean ): Boolean =
			this match
			{
				case SNil => false
				case SPair( head, tail: SList ) =>
					if (p(head))
						true
					else
						tail exists p
				case SPair( left, right ) => sys.error( s"SList.exists: improper list: ($left $right)" )
			}
		
		def forall( p: Any => Boolean ): Boolean =
			this match
			{
				case SNil => true
				case SPair( head, tail: SList ) =>
					if (p(head))
						tail forall p
					else
						false
				case SPair( left, right ) => sys.error( s"SList.forall: improper list: ($left $right)" )
			}
			
		def isProperList: Boolean =
			this match
			{
				case SNil => true
				case SPair( _, tail: SList ) => tail.isProperList
				case _ => false
			}
			
		def toList = SList.toScalaList( this )
		
		def toProperList =
			toList match
			{
				case Left( _ ) => sys.error( "SList.toProperList: list is improper" )
				case Right( list ) => list
			}
	}
case object SNil extends SList
	{
		def head = sys.error( "SList: empty list has no head element" )

		def tail = sys.error( "SList: empty list has no tail element" )
		
		val isEmpty = true
		
		override def toString = "()"
	}
case class SPair( val head: Any, val tail: Any ) extends SList
	{
		val isEmpty = false
		
		override def toString =
			toList match
			{
				case Left( (l, r) ) => l.mkString( "(", " ", "" ) + s" . $r)" 
				case Right( l ) => l.mkString( "(", " ", ")" )
			}
	}

object SList// extends App
{
// 	SList(1, 2) match
// 	{
// 		case SList(a, b) => println(a, b)
// 	}

//	println( toScalaList(SList(1, 2, 3)) )
	
	def apply( elems: Any* ) = fromScalaSeq( elems )
	
	def unapplySeq( list: SList ): Option[Seq[Any]] =
		toScalaList( list ) match
		{
			case Left( _ ) => None
			case Right( list ) => Some( list )
		}
	
	def toScalaList( l: SList ) =
	{
	val buf = new ListBuffer[Any]
	
		def build( rest: SList ): Either[(List[Any], Any), List[Any]] =
			rest match
			{
				case SNil => Right( buf.toList )
				case SPair( head, tail: SList ) =>
					buf += head
					build( tail )
				case SPair( left, right ) =>
					buf += left
					Left( buf.toList, right )
			}
			
		build( l )
	}
	
	def fromScalaSeq( s: Seq[Any] ) =
	{
	val it = s.reverseIterator
	
		def build( tail: SList ): SList =
			if (it.hasNext)
				build( SPair(it.next, tail) )
			else
				tail
		
		build( SNil )
	}
}
SScheme
=======

SScheme is a small Scheme interpreter in Scala intended to be used as an internal DSL/scripting language.

The following is a simple example of how to use it in a Scala program. The Scheme code is from the book "The Scheme Programming Language, Third Edition" in the "Extended Examples" (section 9.2).

	import ca.hyperreal.sscheme

	object Main extends App
	{
		val env = environment(
			"""
			(define sort #f)
			(let ()
				(define dosort
					(lambda (pred? ls n)
						(if (= n 1)
							(list (car ls))
							(let ((i (quotient n 2)))
								(merge pred?
									(dosort pred? ls i)
									(dosort pred? (list-tail ls i) (- n i)))))))
				(define merge
					(lambda (pred? l1 l2)
						(cond
							((null? l1) l2)
							((null? l2) l1)
							((pred? (car l2) (car l1))
							(cons (car l2) (merge pred? l1 (cdr l2))))
							(else (cons (car l1) (merge pred? (cdr l1) l2))))))
				(set! sort
					(lambda (pred? l)
						(if (null? l) l (dosort pred? l (length l))))))
			""" )
		val l = List( 5, 7, 3, 9, 2, 1, 6 )
		
		println( interpret( """ (sort < l) """, env add ('l -> l) ) )
		println( interpret( """ (sort > l) """, env add ('l -> l) ) )
	}

The output is:

	List(1, 2, 3, 5, 6, 7, 9)
	List(9, 7, 6, 5, 3, 2, 1)

	
## License

SScheme is distributed under the MIT License, meaning that you are free to use it in your free or proprietary software.


## Usage

Use the following elements to use SScheme in your Maven project:

	<repository>
		<id>hyperreal</id>
		<url>https://dl.bintray.com/edadma/maven</url>
	</repository>

	<dependency>
		<groupId>ca.hyperreal</groupId>
		<artifactId>sscheme</artifactId>
		<version>0.1</version>
	</dependency>

Add the following to your `build.sbt` file to use SScheme in your SBT project:

	resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

	libraryDependencies += "ca.hyperreal" %% "sscheme" % "0.1"
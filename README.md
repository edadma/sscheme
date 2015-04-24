SScheme
=======

SScheme is a small Scheme interpreter in Scala intended to be used as an internal DSL/scripting language.

Here is a simple example of how to use it in a Scala program (the code is from the book, "The Scheme Programming Language"):

	import ca.hyperreal.sscheme

	object Main extends App
	{
		val env = environment( """
			(define divisors
				(lambda (n)
					(let f ((i 2))
					(cond
						((>= i n) '())
						((integer? (/ n i))
							(cons i (f (+ i 1))))
						(else (f (+ i 1))))))) """ )
		
		println( interpret( """ [divisors 5] """, env ) )
		println( interpret( """ [divisors 32] """, env ) )
	}

The output is:

	List()
	List(2, 4, 8, 16)

	
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
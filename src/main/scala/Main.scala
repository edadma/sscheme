package ca.hyperreal.sscheme


object Main extends App
{
	println( interpret( """ [if [< 1 2] "yes" "no"] """ ) )
	println( interpret( """ [[lambda [x y] [+ x y]] 3 4] """ ) )
	println( interpret( """ [[lambda x x]] """ ) )
 	println( interpret( """ [define f [lambda [x y] [+ [* x x] [* y y]]]] [define a 5] [f a 4] """ ) )
}
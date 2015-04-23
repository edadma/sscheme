package ca.hyperreal.sscheme


object Main extends App
{
	println( interpret( """ [define x 5] [display x] [set! x 6] [display x] [+ x 1] """ ) )
}
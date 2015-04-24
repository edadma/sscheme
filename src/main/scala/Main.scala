package ca.hyperreal.sscheme


object Main extends App
{
	val env = environment(
		"""
		[define s [string-port ""]]
		""" )
	
	println( interpret( """ [display "as\\df"] """, env ) )
}
Ian Delbridge
CSC 173
Brown
11/10/14

Project_KB.pl -- the main, stable version of my program. Should be used for translation
		from a parse tree to FOL.
Project_KB_Develop.pl -- the "developer" version. This has a few extra features, only in
		parsing. That is, it recognizes when to use "a" versus "an", and when
		to use "who/whom" versus "which/that". It also recognizes when to use
		"who" versus "whom" but I cannot recall whether that is in the main 
		version or not. It may very well be but not debugged in that sense.
		This version has some bugs it appears for translation to FOL, so it
		shoudldn't be used for such until all of the kinks are worked out.

To Run: (Parse english into a parse tree and determine if it is grammatical)
	Use the predicate interpret_as_tree/1. This will prompt for an input sentence
	and will then parse it into a tree if it is grammatical. Otherwise it will fail.
	This can also be done manually by using the archetype of 
	"sentence([a, boy, who, likes, watermelons, eats, watermelons],Tree,[])."
	This will bind "Tree" to the parse tree.
ex: interpret_as_tree(Tree).
:"The boy runs."
Tree = sentence(noun_phrase(determiner(exists), noun(boy)), verb_phrase(verb(runs))) .

To Run: (Parse english into a parse tree, determine if it is grammatical, and translate
	to FOL)
	Use the predicate interpret_as_logic/1. This will prompt for an input sentence 
	and will then parse it into a tree and translate that into FOL. This can be
	done manually by following the archetype,
	"sentence([a,boy,runs],Tree,[]),s(Tree,FOPC)."
	This will bind Tree to the parse tree and FOPC to the logical representation of
	the sentence.
	Note that s/2 can be used separately for any pre-made parse trees in the first
	argument.
ex: interpret_as_logic(Logic).
:"The girls run."
Logic = exists(x1, girl(x1)&run(x1)) 



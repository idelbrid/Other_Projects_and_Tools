:-op(500,xfy,& ).
:-op(600,xfy,=> ).
/* Code from the section 5.3 of the textbook,
   Programming in Prolog by Clocksin & Mellish.

   The function read_in(S) takes a raw sentence via
   the standard input, and the sentence's list form
   is assigned to the variable S. For example, with
   the input
      A watermelon contains a divine flavor!
   we will obtain
      S = [a,watermelon,contains,a,divine,flavor,!]
   so that we don't have to bother ourselves with
   converting a sentence into a list whenever
   we try to parse it. Note that all capial letters
   become lowercase. Also, it assumes the input
   sentence ends with either '.', '?', or '!'. */

read_in([W|Ws]) :- get_char(C),
                   readword(C,W,C1),
                   restsent(W,C1,Ws).

restsent(W, _, []) :- lastword(W),!.
restsent(_, C, [W1|Ws]) :- readword(C,W1,C1),
	                   restsent(W1,C1,Ws).

readword(C,C,C1) :- single_character(C),!,get_char(C1).
readword(C,W,C2) :-
	in_word(C, NewC),
	!,
	get_char(C1),
	restword(C1,Cs,C2),
	atom_chars(W,[NewC|Cs]).
readword(_,W,C2):-get_char(C1),readword(C1,W,C2). % skipping blanks.

restword(C,[NewC|Cs],C2) :-
	in_word(C,NewC),
	!,
	get_char(C1),restword(C1,Cs,C2).
restword(C,[],C).

in_word(C,C) :- letter(C,_).
in_word(C,L) :- letter(L,C).
in_word(C,C) :- digit(C).
in_word(C,C) :- special_character(C).

special_character('-').
special_character('''').

single_character(',').          single_character(':').
single_character('.').          single_character('?').
single_character(';').          single_character('!').

letter(a,'A').		  letter(n,'N').
letter(b,'B').		  letter(o,'O').
letter(c,'C').		  letter(p,'P').
letter(d,'D').		  letter(q,'Q').
letter(e,'E').		  letter(r,'R').
letter(f,'F').		  letter(s,'S').
letter(g,'G').		  letter(t,'T').
letter(h,'H').		  letter(u,'U').
letter(i,'I').		  letter(v,'V').
letter(j,'J').		  letter(w,'W').
letter(k,'K').		  letter(x,'X').
letter(l,'L').		  letter(y,'Y').
letter(m,'M').		  letter(z,'Z').

digit('0').        digit('5').
digit('1').        digit('6').
digit('2').        digit('7').
digit('3').        digit('8').
digit('4').        digit('9').

lastword('.').
lastword('!').
lastword('?').

sentence --> sentence(X, T).
sentence(Sentence, Tree, []) :- sentence(_, Tree, Sentence, []).
sentence(X, sentence(NP, VP)) --> noun_phrase(X, NP), verb_phrase(X, VP).
sentence(X, sentence(NP, VP)) --> noun_phrase(X,NP), verb_phrase(X, VP), finisher.

noun_phrase(plural, noun_phrase(N)) --> noun(plural, N).
noun_phrase(plural, noun_phrase(A,N)) --> adjective(A), noun(plural, N).
noun_phrase(plural, noun_phrase(N, RC )) --> noun(plural, N), 
											relative_clause(plural, RC).
noun_phrase(plural, noun_phrase(A, N, RC)) --> adjective(A), noun(plural, N),
											relative_clause(plural, RC).
											
											
noun_phrase(X, noun_phrase(D, N)) --> determiner(X, D), noun(X, N).
noun_phrase(X, noun_phrase(D, A, N)) --> determiner(X, D), adjective(A), noun(X, N).
noun_phrase(X, noun_phrase(D, N, RC )) --> determiner(X, D), noun(X, N), 
											relative_clause(X, RC).
noun_phrase(X, noun_phrase(D, A, N, RC)) --> determiner(X,D), adjective(A), noun(X, N),
											relative_clause(X, RC).
verb_phrase(X, verb_phrase(V, NP)) --> verb(X, V), noun_phrase(Y, NP).
verb_phrase(X, verb_phrase(V) ) --> verb(X, V).
verb_phrase(X, verb_phrase(LV, NP)) --> linking_verb(X, LV), noun_phrase(X,NP).
verb_phrase(X, verb_phrase(LV,A)) --> linking_verb(X, LV), adjective(A).

relative_clause(X, relative_clause(Rel, VP)) --> relative_pronoun(X, Rel), 
													verb_phrase(X, VP).
relative_clause(X, relative_clause(Rel, NP, VP))--> relative_pronoun(X, Rel),
													noun_phrase(Y, NP),
													verb_phrase(Y, VP).

determiner( _, determiner(exists) ) --> [the].
determiner( singular, determiner(exists) ) --> [a].
determiner( singular, determiner(exists) ) --> [an].
determiner( _ , determiner(all) ) --> [all].
determiner( _ , determiner(all) ) --> [any].
determiner( _, determiner(exists) ) --> [some].
determiner( _, determiner(all) ) --> [every].


adjective( adjective(A)) --> [A], {is_adjective(A)}.

finisher --> [!].
finisher --> [.].
finisher --> [?].


is_adjective(divine).
is_adjective(good).
is_adjective(tasty).
is_adjective(evil).
is_adjective(pacifist).
is_adjective(big).
is_adjective(delicious).

noun( X, noun(N) ) --> [N], {is_noun(X, N)}.
noun( plural, noun(Nsing) ) --> [N], {is_noun(plural, N, Nsing)}.

is_noun(singular, boy).
is_noun(plural, boys, boy).
is_noun(singular, girl).
is_noun(plural, girls, girl).
is_noun(singular, pickle).
is_noun(plural, pickles, pickle).
is_noun(singular, person).
is_noun(plural, people, person).
is_noun(singular, flavor).
is_noun(plural, flavors, flavor).
is_noun(singular, government).
is_noun(plural, governments, governments).
is_noun(singular, watermelon).
is_noun(plural, watermelons, watermelon).
is_noun(singular, apple).
is_noun(plural, apples, apple).


verb( X, verb(V) ) --> [V], {is_verb(X, V)}.

is_verb(singular, runs).
is_verb(plural, run).
is_verb(singular, eats).
is_verb(plural, eat).
is_verb(singular, likes).
is_verb(plural, like).
is_verb(singular, eats).
is_verb(plural, eat).
is_verb(singular, conscripts).
is_verb(plural, conscript).
is_verb(singular, contains).
is_verb(plural, contain).
is_verb(singular, has).
is_verb(plural, have).

linking_verb(X, linking_verb(LV)) --> [LV], {is_linking_verb(X,LV)}.
linking_verb(X, linking_verb(LV1)) --> [LV], {is_linking_verb(X,LV,LV1)}.
is_linking_verb(singular, is).
is_linking_verb(plural, are,is).

relative_pronoun(X, relative_pronoun(RP)) --> [RP], {is_relative_pronoun(X, RP)}.

is_relative_pronoun(_, who).
is_relative_pronoun(_, which).
is_relative_pronoun(_, that).
is_relative_pronoun(_, whom).



np(Var, noun_phrase(DET, ADJ, N ), P3) :- 
		det(Var, DET, P1, P2, P3),
		adj(Var, ADJ, P1),
		n(Var, N, P2).
np(Var, noun_phrase(DET, N ), P2) :- 
		det(Var, DET, P1, P2),
		n(Var, N, P1).
np(Var, noun_phrase(DET, N, RC ), P3 ):- 
		det(Var, DET, P2, P1, P3),
		n(Var, N, P1),
		rel(Var, RC, P2).

		
rel(Var, relative_clause(Rel, NP, VP), all(A,B=>P)) :-
		print(problem_in_rel1),
		Rel =.. [relative_pronoun,_],
		gensym(x,X),
		VP =.. [verb_phrase, V],
		V =.. [verb, Verb],
		P =.. [Verb, X, Var],
		np(X,NP,all(A,B)).

rel(Var, relative_clause(Rel, NP, VP), exists(A,B&P)) :-
		print(problem_in_rel2),
		Rel =.. [relative_pronoun,_],
		gensym(x,X),
		VP =.. [verb_phrase, V],
		V =.. [verb, Verb],
		P =.. [Verb, X, Var],
		np(X,NP,exists(A,B)).
		
rel(Var, relative_clause(Rel, VP), P3) :-
		print(problem_in_rel3),
		Rel =.. [relative_pronoun, _],
		vp(Var, VP, P3).
		
det(Var, determiner(all), P1, P2, all(Var, (P2 & P1) ) ).
det(Var, determiner(all), P1, all(Var, P1 )). 

det(Var, determiner(exists), P1, P2, exists(Var, (P2 & P1 ))).
det(Var, determiner(exists), P1, exists(Var, P1)).

adj(Var, DESC, P):- 
		DESC =.. [adjective, ADJ], /** adjective(big) notation to big(x) */
		P =.. [ADJ, Var].
		
n(Var, DESC, P) :- 
		DESC =.. [noun, N], /** noun(apple) notation to apple(x) */
		P =.. [N, Var].
		
v(Var, DESC, P) :- 
		DESC =.. [verb, V], /** verb(runs) notation to runs(x) */ 
		P =.. [V, Var].
		
v(Var, DESC_V, P_noun, P) :- 
		DESC_V =.. [verb, V],
		P =.. [V, Var, P_noun].

vp(Var, verb_phrase(LV,ADJ), C) :-
		print(problem_in_vp1),
	LV =.. [linking_verb,_],
	adj(Var,ADJ,C).
vp(Var, verb_phrase(V), C) :- 
		print(problem_in_vp2),
	v(Var, V, C).
vp(Var, verb_phrase(V, NP), C) :- 
		print(problem_in_vp3),
	gensym(x,X),
	np(X,NP,T),
	v(Var,V,T,C).

	
		
vp(Var,all(A,B),verb_phrase(LV,ADJ), all(A,B => C)) :-
	LV =.. [linking_verb,_], %verifying and unpacking linking_verb(BE) 
	adj(Var,ADJ,C). %calling adj above again to go from adjective(delicious) 
					%to delicious(x) 
vp(Var,exists(A,B), verb_phrase(LV, ADJ), exists(A,B&C)) :- 
	LV =.. [linking_verb,_],
	adj(Var,ADJ, C).
	
%vp(Var, all(A,B), verb_phrase(LV, NP), all(A,B => C)) :- 
%	LV =.. [linking_verb,_],
%	noun_phrase(
%	n(Var,N, C).
%vp(Var, exists(A,B), verb_phrase(LV, NP), exists(A,B&C)) :- 
%	LV =.. [linking_verb,_],
%	n(Var,N, C).

vp(Var, all(A,B), verb_phrase(V), all(A,B => C)) :- 
	v(Var, V, C).
vp(Var, exists(A,B), verb_phrase(V), exists(A,B&C)) :- 
	v(Var, V, C).

vp(Var, all(A,B), verb_phrase(V, NP), all(A,B => C)) :- 
	gensym(x,X),
	np(X,NP, T), 
	v(Var, V, T, C).
vp(Var, exists(A,B), verb_phrase(V, NP), exists(A,B&C)) :- 
	gensym(x,X),
	np(X,NP,T),
	v(Var,V,T,C).
	
	
	

	
%%Create an "all" and an "exists" interpretation for each noun_phrase above
%%Create an "all" and an "exists" interpretation for each verb_phrase above	
	
	
s(sentence(NP,VP), F) :- 
	!,
	gensym(x,X),
	np(X,NP,T), vp(X,T,VP,F).
	
interpret_as_tree(Tree) :- 
	read_in(S),
	sentence(S,Tree,[]).
	
interpret_as_logic(FOPC) :- 
	read_in(S),
	sentence(S,Tree,[]),
	s(Tree,FOPC).
	
	
				
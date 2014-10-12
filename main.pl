%
%		README
%		
%		Start by calling main, in any of the following ways:
%
%		1. "main."
%		2. "main([semantics])."
%		3. "main([syntax])."
%		4. "main([])."
%
%		The arguments given determines what output the program will give.
%
%
%
%
%
%	
%		Then write statements or questions like:
%
%		1. peter has a cat
%		2. lisa s cat is called johny			(the s in "lisa's" must be separated like so)
%		3. where did peter meet lisa?
%
%
%
%		You can also write "list" to have all stored facts printed
%
%
%
%		To clear the facts, prolog must be restarted.








:- use_module(write_predicates).
:- use_module(grammar).
:- use_module(semantics).
:- use_module(mylists).

% SHORTCUTS
a:- consult(main), consult(write_predicates), consult(grammar), consult(semantics), consult(mylists).

:- 
	dynamic property/2, 
	possess/2,
	action/2,
	actor/2,
	receiver/2,
	be/2,
	location/2,
	called/2.

%% be(X, X). %common sense


stored_fact(called(A, B)):- clause(called(A, B), _).
stored_fact(be(A, B)):- clause(be(A, B), _).
stored_fact(location(A, B)):- clause(location(A,B), _).
stored_fact(action(A, B)):- clause(action(A, B), _).
stored_fact(actor(A, B)):- clause(actor(A, B), _).
stored_fact(receiver(A, B)):- clause(receiver(A, B), _).

main:- main([semantics]).

% Flags:
% 1. [semantics]
% 2. [syntax]
% 3. []
main(Flags):-
	main_loop(Flags).

main_loop(Flags):-
	read_input_words(Words),
	!,
	handle_bye_input(Words),
	(
		handle_list_facts(Words)
	;
		parse_input(Words, Flags)
	),
	!,
	main_loop(Flags).

handle_bye_input([bye|_]):- !, fail.
handle_bye_input(_):- !.

%if user writes "list", print all facts, otherwise fail.
handle_list_facts([list]):-
	findall(Fact, stored_fact(Fact), Facts),
	write_list(Facts), nl.

parse_input(Words, Flags):-
	write("parsing input..."), 
	generate_syntax_trees(Words, SyntaxTrees),
	write("  [X]"), nl, 
	write("parsing syntax trees..."),
	!,
	maplist(parse_syntax_tree, SyntaxTrees, SemanticsLists, IsQuestionLists, QueriedVarsLists),
	write("  [X]"), nl,
	!,
	length(SyntaxTrees, NumTrees),
	!,
	nl, nl, writef("# generated syntax trees: %t", [NumTrees]), nl, nl,
	write_output(SyntaxTrees, SemanticsLists, Flags),
	!,
	(
		NumTrees == 1,
		!,
		SemanticsLists = [FirstSemantics | _ ],
		IsQuestionLists = [FirstIsQuestion | _ ],
		QueriedVarsLists = [FirstQueriedVars | _],
		process_semantics(FirstSemantics, FirstIsQuestion, FirstQueriedVars)
	;
		NumTrees > 1,
		write("Choose interpretation. Write a number!"), nl,nl,
		read_until_input_number_between(Number, 1, NumTrees),
		!,
		Index is Number - 1,
		nth0(Index, SemanticsLists, Semantics),
		nth0(Index, IsQuestionLists, IsQuestion),
		nth0(Index, QueriedVarsLists, QueriedVars),
		process_semantics(Semantics, IsQuestion, QueriedVars),
		!
	;
		NumTrees == 0,
		!,
		write_cyan_bold("     Could not parse the input. Try again!"),
		nl, nl
	),
	!.

read_until_input_number_between(Number, MinIncl, MaxIncl):-
	read_line_to_codes(user_input, [NumberCode]),
	Number is NumberCode - 48, %From ascii to number
	Number >= MinIncl,
	Number =< MaxIncl
	;
	read_input_number(Number, MinIncl, MaxIncl). % Try until success

% no semantics to parse, doesn't matter if it's a question or statement
process_semantics([], _, _QueriedVars):-
	write_cyan_bold("     Sorry, no semantics generated from syntax tree!"),
	nl, nl.

process_semantics(Semantics, false, _QueriedVars):-
	(
		not_already_known(Semantics),
		maplist(replace_underscore, Semantics, Tmp),
		length(Tmp, NumSemantics),
		take_n(Tmp, NumSemantics, DuplicatedSemantics),
		maplist(replace_args_with_known_entities, Tmp, DuplicatedSemantics, FormattedFacts),
		maplist(assert_if_new, FormattedFacts, Output),
		write_cyan_bold("     Roger that!"), nl,
		nl, write("Asserting: "), write_list(Output)
	;
		write_cyan_bold("     I already know that!"),	
		true
	),
	nl, nl.

process_semantics(Semantics, true, QueriedVars):-
	answer_question(Semantics, QueriedVars),
	nl, nl.

assert_if_new(Fact, Fact):-
	not_already_known([Fact]),
	!,
	assert(Fact).

assert_if_new(_, ""):-
	!.

not_already_known(Semantics):-
	maplist(call, Semantics),
	!,
	fail.

not_already_known(_):-
	!, 
	true.

answer_question(Queries, QueriedVars):-
	write("Queried vars:   "), write(QueriedVars), nl,nl,
	has_positive_answer(Queries),
	!,
	(
		maplist(call, Queries),
		(
			length(QueriedVars, 0),
			write_cyan_bold("     Yes!")
		;
			maplist(explain, QueriedVars, Explanations),
			nl, write_list(Explanations), 
			fail
		)
	;
		true
	),
	!.

answer_question(_, _):-
	!,
	write_cyan_bold("     I can't say for sure!").

has_positive_answer(Queries):-
	copy_term(Queries, QueriesCopy),
	maplist(call, QueriesCopy).


explain(X, Name):-
	called(X, Name),
	!.

explain(X, Explanation):-
	action(Action, possess),
	receiver(Action, X),
	actor(Action, Possesser),
	explain(Possesser, PossesserExplanation),
	be(X, Possession),
	X \= Possession,
	string_concat(PossesserExplanation, "s ", Possessers),
	string_concat(Possessers, Possession, Explanation),
	!.

explain(X, Explanation):-
	be(X, WhatXIs),
	X \= WhatXIs,
	explain(WhatXIs, Explanation),
	!.

explain(X, X):-
	not(generated_atom(X)).

generated_atom(Atom):-
	LowerCaseX = 120,
	atom_codes(Atom, [LowerCaseX | _]).
	

replace_args_with_known_entities(Semantic, AllSemantics, NewSemantic):-
	arg(1, Semantic, Arg1),
	arg(2, Semantic, Arg2),
	replace_arg_with_known_entity(Arg1, AllSemantics, NewArg1),
	replace_arg_with_known_entity(Arg2, AllSemantics, NewArg2),
	functor(Semantic, FactName, 2),
	functor(NewSemantic, FactName, 2),
	arg(1, NewSemantic, NewArg1),
	arg(2, NewSemantic, NewArg2).

replace_arg_with_known_entity(Arg, AllSemantics, KnownEntity):-
	member(called(Arg, Name), AllSemantics),
	%% called(Arg, Name),
	called(KnownEntity, Name),
	KnownEntity \= Arg,
	!.

replace_arg_with_known_entity(Arg, _, Arg):-
	!.



replace_underscore(Input, Result):-
	Underscore = 95,
	LowerCaseX = 120,
	replace_in_term(Underscore, Input, LowerCaseX, Result).

write_output(SyntaxTrees, SemanticsLists, Flags):-
	write_output(SyntaxTrees, SemanticsLists, Flags, 1).

write_output([SyntaxTree | RestOfTrees], [Semantics | RestOfSemantics], Flags, TreeNumber):-
	ansi_format([fg(cyan), bold], '~w~w~w', [" ----------------- ", TreeNumber, " --------------------"]),
	nl,
	write_syntax_and_semantics(SyntaxTree, Semantics, Flags),
	NextTreeNumber is TreeNumber + 1,
	write_output(RestOfTrees, RestOfSemantics, Flags, NextTreeNumber).

write_output([], [], _, _):-
	write_cyan_bold(" -----------------------------------------"),
	nl, nl.

write_syntax_and_semantics(SyntaxTree, Semantics, []):-
	!,
	write("Syntax: "), nl, write_compound(SyntaxTree), nl, nl,
	write("Semantics: "), nl, write_list(Semantics), nl.

write_syntax_and_semantics(SyntaxTree, _Semantics, [syntax]):-
	!,
	write("Syntax: "), nl, write_compound(SyntaxTree), nl.

write_syntax_and_semantics(_SyntaxTree, Semantics, [semantics]):-
	!,
	write("Semantics: "), nl, write_list(Semantics), nl.

write_syntax_and_semantics(_, _, Flags):-
	write("Writing failed! Flags: "), write(Flags),
	!,
	fail.	

read_input_words(Words):-
	write_yellow_bold(" > "),
	read_line_to_codes(user_input, Codes),
	insert_space_before_punctuation(Codes, FormattedCodes),
	!, 
	atom_codes(Input, FormattedCodes),
	atomic_list_concat(UnformattedWords, " ", Input),
	delete(UnformattedWords, '', Words).


generate_syntax_trees(Words, SyntaxTrees):-
	findall(Tree, parse_sentence(Words, Tree), SyntaxTrees).

insert_space_before_punctuation([Code|CodesT], [32,Code|NewCodesT]):-
	is_punctuation(Code),
	insert_space_before_punctuation(CodesT, NewCodesT).

insert_space_before_punctuation([Code|CodesT], [Code|NewCodesT]):-
	not(is_punctuation(Code)),
	insert_space_before_punctuation(CodesT, NewCodesT).

insert_space_before_punctuation([], []).

is_punctuation(46). %  .
is_punctuation(33). %  !
is_punctuation(63). %  ?

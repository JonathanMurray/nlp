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
	location/2.

uninteresting_var(X):- action(X, _).
uninteresting_var(X):- actor(X, _).
uninteresting_var(X):- receiver(X, _).

main:- main([]).

main(Flags):-
	main_loop(Flags).

main_loop(Flags):-
	read_input_words(Words),
	!,
	handle_bye_input(Words),
	write("parsing input..."), 
	generate_syntax_trees(Words, SyntaxTrees),
	write("  [X]"), nl, 
	write("parsing syntax trees..."),
	!,
	maplist(parse_syntax_tree, SyntaxTrees, SemanticsLists, IsQuestionLists),
	write("  [X]"), nl,
	!,
	length(SyntaxTrees, NumTrees),
	!,
	nl, nl, writef("# generated syntax trees: %t", [NumTrees]), nl, nl,
	write_output(SyntaxTrees, SemanticsLists, Flags),
	!,
	(
		NumTrees == 1,
		SemanticsLists = [FirstSemantics | _ ],
		IsQuestionLists = [FirstIsQuestion | _ ],
		process_semantics(FirstSemantics, FirstIsQuestion),
		!
	;
		NumTrees > 1,
		write("Choose interpretation. Write a number!"), nl,nl,
		read_until_input_number_between(Number, 1, NumTrees),
		!,
		Index is Number - 1,
		nth0(Index, SemanticsLists, Semantics),
		nth0(Index, IsQuestionLists, IsQuestion),
		process_semantics(Semantics, IsQuestion),
		!
	;
		!,
		write_cyan_bold("     Could not parse the input. Try again!"),
		nl, nl
	),
	!,
	main_loop(Flags).

handle_bye_input([bye|_]):- !, fail.
handle_bye_input(_):- !.

read_until_input_number_between(Number, MinIncl, MaxIncl):-
	read_line_to_codes(user_input, [NumberCode]),
	Number is NumberCode - 48, %From ascii to number
	Number >= MinIncl,
	Number =< MaxIncl
	;
	read_input_number(Number). % Try until success


process_semantics(Semantics, false):-
	(not_already_known(Semantics) ->
		maplist(replace_underscore, Semantics, FormattedFacts),
		maplist(assert, FormattedFacts),
		write_cyan_bold("     Roger that!")
	;	
		true
	),
	nl, nl.

process_semantics(Semantics, true):-
	answer_question(Semantics),
	nl, nl.

not_already_known(Semantics):-
	maplist(call, Semantics),
	!,
	write_cyan_bold("     I already know that!"),
	fail.

not_already_known(_):-
	!, 
	true.

answer_question(Queries):-
	%% write_list(Queries),
	question_vars(Queries, QuestionVars),
	has_positive_answer(Queries),
	!,
	(
		maplist(call, Queries),
		exclude(uninteresting_var, QuestionVars, InterestingQuestionVars),
		(
			length(InterestingQuestionVars, 0),
			write_cyan_bold("     Yes!")
		;
			write("     "), write_cyan_bold(InterestingQuestionVars),
			fail
		)
	;
		true
	),
	!.

answer_question(_):-
	!,
	write_cyan_bold("     I can't say for sure!").

has_positive_answer(Queries):-
	copy_term(Queries, QueriesCopy),
	maplist(call, QueriesCopy).


question_vars(Queries, QuestionVars):-
	maplist(nonground_entities, Queries, ListOfNongroundLists),
	flatten(ListOfNongroundLists, NongroundsWithDuplicates),
	list_to_set(NongroundsWithDuplicates, QuestionVars).
	

	
nonground_entities(Query, Nongrounds):-
	functor(Query, _Name, NumArgs),
	collect_args(NumArgs, Query, [], Args),
	include(not_ground, Args, Nongrounds).


collect_args(ArgNum, Term, Acc, Args):-
	ArgNum > 0,
	arg(ArgNum, Term, CurrentArg),
	ArgNumMinus1 is ArgNum - 1,
	collect_args(ArgNumMinus1, Term, [CurrentArg|Acc], Args).

collect_args(0, _, Acc, Acc).

not_ground(X):- not(ground(X)).


replace_underscore(Input, Result):-
	Underscore = 95,
	LowerCaseX = 120,
	replace(Underscore, Input, LowerCaseX, Result).

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

read_input_words(Words):-
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

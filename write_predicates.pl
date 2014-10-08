:- module(write_predicates, [write_list/1, write2/1, nl2/0, write_compound/1, write_cyan_bold/1, write_vars/1]).

write_list(List):- write_list(List, 0), !.

write_list(FaultyList):- 
	write("ERROR! Failed to write list properly..."), 
	nl,
	write("Trying regular write instead:"),
	nl,
	write(FaultyList), !.

write_list(List, Indent):-
	tab(Indent),
	write("["), 
	nl,
	ElementIndent is Indent + 2,
	write_elements(List, ElementIndent),
	tab(Indent),
	write("]").

write_elements([H|T], Indent):-
	is_list(H),
	!,
	write_list(H, Indent),
	nl,
	write_elements(T, Indent).

write_elements([H|T], Indent):-
	not(is_list(H)),
	!,
	tab(Indent),
	write(H),
	nl,
	write_elements(T, Indent).

write_elements([], _):- !.

write_compound(Compound):-
	write_compound(Compound, 0).

write_compound(Compound, Indent):-
	compound(Compound),
	compound_name_arguments(Compound, Name, Args),
	tab(Indent), write(Name), write("("), nl,
	ArgIndent is Indent + 3,
	write_compound_elements(Args, ArgIndent),
	tab(Indent), write(")").

write_compound(Terminal, Indent):-
	not(compound(Terminal)),
	tab(Indent),
	ansi_format([bold,fg(cyan)], '~w', [Terminal]).

write_compound_elements([H|T], Indent):-
	write_compound(H, Indent),
	nl,
	write_compound_elements(T, Indent).

write_compound_elements([], _):- !.


debug_enabled:- 
%	fail.
	true.

write2(Text):- debug_enabled, !, write(Text).
write2(_):- not(debug_enabled), !.
nl2:- debug_enabled, !, nl.
nl2:- not(debug_enabled), !.


write_cyan_bold(Term):-
	ansi_format([fg(cyan), bold], '~w', [Term]).


write_vars(Compound):-
	compound(Compound),
	functor(Compound, _Name, Arity),
	write_args(Arity, Compound).

write_args(Arg, Term):-
	Arg > 0,
	arg(Arg, Term, Value),
	write(Value), 
	nl,
	ArgMinus1 is Arg - 1,
	write_args(ArgMinus1, Term).

write_args(0, _Term).
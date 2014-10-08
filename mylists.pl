:- module(mylists, [delete_ground/3, replace/4]).

delete_ground([H|T], Element, NewT):-
	ground(H),
	Element = H, 
	!,
	delete_ground(T, Element, NewT).

delete_ground([H|T], Element, [H|NewT]):-
	!,
	delete_ground(T, Element, NewT).

delete_ground([], _, []).

replace(Occurrence, Input, Replacement, Result):-
	term_to_atom(Input, InputAtom),
	atom_codes(InputAtom, InputCodes),
	(	
		select(Occurrence, InputCodes, Replacement, ResultCodes);
		not(member(Occurrence, InputCodes)), ResultCodes = InputCodes
	),
	atom_codes(ResultAtom, ResultCodes),
	term_to_atom(Result, ResultAtom).
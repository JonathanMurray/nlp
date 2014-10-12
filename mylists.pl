:- module(mylists, [replace_in_term/4, take_n/3]).

replace_in_term(Occurrence, Input, Replacement, Result):-
	term_to_atom(Input, InputAtom),
	atom_codes(InputAtom, InputCodes),
	(	
		replace_all(Occurrence, InputCodes, Replacement, ResultCodes);
		not(member(Occurrence, InputCodes)), ResultCodes = InputCodes
	),
	atom_codes(ResultAtom, ResultCodes),
	term_to_atom(Result, ResultAtom).

replace_all(Occurrence, [Occurrence | T], Replacement, [Replacement | ResultT]):-
	replace_all(Occurrence, T, Replacement, ResultT).

replace_all(Occurrence, [H | T], Replacement, [H | ResultT]):-
	H \= Occurrence,
	replace_all(Occurrence, T, Replacement, ResultT).

replace_all(_, [], _, []).

% take_n(+Element, +N, -ResultList)
take_n(Element, N, [Element | T]):-
	N > 0,
	NMinus1 is N -1,
	take_n(Element, NMinus1, T).

take_n(_, 0, []).
	
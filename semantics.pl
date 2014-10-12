:- module(semantics, [parse_syntax_tree/4]).


% parse_syntax_tree(+Syntax, -Semantics, -IsQuestion)
%
%	Parse syntax_tree (that represents a sentence) 
%	and produce the semantic meaning. The meaning is
%	a list of relations (facts).
% 	The value of IsQuestion shows if the given sentence
%	is a question (rather than a statement).

parse_syntax_tree(s(NP, VP), Semantics, false, Queried):-
	parse_np(NP, Subject, SideEffects, Queried1),
	parse_vp(VP, Subject, SemanticsVP, _ActionID, Queried2),
	append(Queried1, Queried2, Queried),
	append(SideEffects, SemanticsVP, Semantics),
	!.
	
parse_syntax_tree(s(S1, cc(_), S2), Semantics, false, Queried):-
	parse_syntax_tree(S1, Semantics1, false, Queried1),
	parse_syntax_tree(S2, Semantics2, false, Queried2),
	append(Queried1, Queried2, Queried),
	append(Semantics1, Semantics2, Semantics),
	!.

parse_syntax_tree(s(aux(_Aux), NP, VP), Semantics, true, Queried):-
	parse_syntax_tree(s(NP, VP), Semantics, _, Queried). %Parse as if it were a statement

parse_syntax_tree(s(NPObj, aux(_Aux), NPSubj, VP), Semantics, true, Queried):-
	merge(VP, NPObj, NewVP),
	parse_syntax_tree(s(NPSubj, NewVP), Semantics, _, Queried). %Parse as if it were a statement

parse_syntax_tree(s(PP, aux(_Aux), NP, VP), Semantics, true, Queried):-
	parse_syntax_tree(s(NP, vp(VP, PP)), Semantics, _, Queried). %Parse as a statement

parse_syntax_tree(s(vp(NPObj, verb(be)), NPSubj), Semantics, true, Queried):-
	parse_syntax_tree(s(NPSubj, vp(verb(be), NPObj)), Semantics, _, Queried). %Parse as statement

parse_syntax_tree(s(PP, VP, NP), Semantics, true, Queried):-
	parse_syntax_tree(s(NP, vp(VP, PP)), Semantics, _, Queried). %Parse as statement


parse_syntax_tree(question(S), Semantics, true, Queried):-
	parse_syntax_tree(S, Semantics, _, Queried),
	!.



parse_syntax_tree(_BadTree, [], false, []):-
	nl,write("Couldn't parse syntax tree!").
	



% merge(+VP, +AddToEndOfVP, -NewVP)

merge(vp(Verb), NP, vp(Verb, NP)):-
	functor(NP, np, _).
merge(vp(VP, PP), NP, vp(MergedVP, PP)):-
	merge(VP, NP, MergedVP).


merge(VP, NP, Merged):-
	nl, write("merge("), write(VP), write(",  "), write(NP), write(",  "), write(Merged), write(")   failed!"),
	fail.





% --------------------------------------------------------------------%
% ----------------------        VP        ----------------------------%
% ---------------------------------------------	----------------------%

% parse_vp(+VP_Tree, -SubjectEntityID, -SemanticMeaning, -ActionID)
%
%	Parse VP subtree and produce 
%	an atom representing the subject of the VP,
%	a list of semantic relations,
%	and an atom representing the action of the VP.


% is a dog
parse_vp(vp(verb(Verb), NP), Subject, [be(Subject, Object) | SideEffects], _, Queried):-
	being_verb(Verb),
	parse_np(NP, Object, SideEffects, Queried).

% has a dog
parse_vp(vp(verb(Verb), NP), Subject, Semantics, ActionID, Queried):-
	not(being_verb(Verb)),
	parse_np(NP, Object, SideEffects, Queried),
	generate_action(ActionID, Verb, Subject, Object, SemanticsH),
	append(SemanticsH, SideEffects, Semantics).

% greets lisa politely
parse_vp(vp(verb(Verb), NP, adverb(Adverb)), Subject, Semantics, ActionID, Queried):-
	not(being_verb(Verb)),
	parse_np(NP, Object, SideEffects, Queried),
	generate_action(ActionID, Verb, Subject, Object, ActionSem),
	append(SideEffects, [property(ActionID, Adverb) | ActionSem], Semantics).

% laughs loudly
parse_vp(vp(verb(Verb), adverb(Adverb)), Subject, Semantics, ActionID, []):-
	generate_action(ActionID, Verb, Subject, noone_, ActionSem),
	Semantics = [property(ActionID, Adverb) | ActionSem].

% is cute
parse_vp(vp(verb(Verb), adj(Adjective)), Subject, [property(Subject, Adjective)], _, []):-
	being_verb(Verb).

% laughs
parse_vp(vp(verb(Verb)), Subject, Semantics, ActionID, []):-
	not(being_verb(Verb)),
	generate_action(ActionID, Verb, Subject, noone_, Semantics),
	!.

% is called peter
parse_vp(vp(verb(be), verb(call), nnp(Name)), Subject, [called(Subject, Name)], _ActionID, []).

% is in the park
parse_vp(vp(vp(verb(Verb)), PP), Subject, Semantics, _, Queried):-
	being_verb(Verb),
	parse_pp(PP, Subject, Semantics, Queried).

% walks in the park
parse_vp(vp(VP, PP), Subject, Semantics, ActionID, Queried):-
	parse_vp(VP, Subject, SemanticsVP, ActionID, []),
	parse_pp(PP, ActionID, SemanticsPP, Queried),
	append(SemanticsVP, SemanticsPP, Semantics).


generate_action(ActionID, Verb, Actor, Receiver, Semantics):-
	generate_actor(ActionID, Actor, SemActor),
	generate_receiver(ActionID, Receiver, SemReceiver),
	append([action(ActionID, Verb)], SemActor, Tmp),
	append(Tmp, SemReceiver, Semantics).


generate_actor(ActionID, Actor, [actor(ActionID, Actor)]):-
	var(Actor), %Unbound Actor comes from a questioning wh-word in input 
	!.

generate_actor(_ActionID, noone_, []).
	
generate_actor(ActionID, and(Actor1, Actor2), Sem):-
	generate_actor(ActionID, Actor1, Sem1),
	generate_actor(ActionID, Actor2, Sem2),
	append(Sem1, Sem2, Sem).

generate_actor(ActionID, Actor, [actor(ActionID, Actor)]).

generate_receiver(ActionID, Receiver, [receiver(ActionID, Receiver)]):-
	var(Receiver), %Unbound Receiver comes from a questioning wh-word in input
	!.

generate_receiver(_ActionID, noone_, []).

generate_receiver(ActionID, and(Receiver1, Receiver2), Sem):-
	generate_receiver(ActionID, Receiver1, Sem1),
	generate_receiver(ActionID, Receiver2, Sem2),
	append(Sem1, Sem2, Sem).

generate_receiver(ActionID, Receiver, [receiver(ActionID, Receiver)]).








% --------------------------------------------------------------------%
% ----------------------        NP        ----------------------------%
% --------------------------------------------------------------------%

% parse_np(+NP_Tree, -EntityID, -SideEffects, -Queried)
%
%	Parse NP subtree and produce an atom representing
%	the main entity of the NP, and also a list of 
%	side effects. 
%	Example: "a black dog" --> entity: dog, side effects: [dog is black]


parse_np(np(wh(who)), Unbound, [], [Unbound]).
parse_np(np(wh(what)), Unbound, [], [Unbound]).

% peter
parse_np(np(nnp(ProperNoun)), EntityID, [called(EntityID, ProperNoun)], []).

% he
parse_np(np(pronoun(Pronoun)), Pronoun, [], []).

% dog
parse_np(np(NN), X, SideEffects, []):-
	parse_nn(NN, X, SideEffects).

% a dog
parse_np(np(dt(_Determiner), NN), X, SideEffects, []):-
	parse_nn(NN, X, SideEffects).

% a dog and peter
parse_np(np(NP1, cc(and), NP2), and(ID1, ID2), SideEffects, []):-
	parse_np(NP1, ID1, SideEffects1, []), %no queries in "x and y"-form allowed.
	parse_np(NP2, ID2, SideEffects2, []),
	append(SideEffects1, SideEffects2, SideEffects).

% a dog in the park / what in the park
parse_np(np(NP, PP), SemanticID, SideEffects, Queried):-
	parse_pp(PP, SemanticID, SemanticsPP, Queried1),
	parse_np(NP, SemanticID, SideEffects2, Queried2), %This order prevents bad recursion(?)
	append(Queried1, Queried2, Queried),
	append(SemanticsPP, SideEffects2, SideEffects).

% peters dog / whos dog 
parse_np(np(NP1, s, NP2), PossessionID, Semantics, Queried):-
	parse_np(NP2, PossessionID, SideEffectsPossession, []), % "peter's what" is not allowed.
	parse_np(NP1, PossesserID, SideEffectsPossesser, Queried),
	generate_action(_ActionID, possess, PossesserID, PossessionID, ActionSemantics),
	flatten([SideEffectsPossession, SideEffectsPossesser, ActionSemantics], Semantics).







% --------------------------------------------------------------------%
% ----------------------        NN        ----------------------------%
% --------------------------------------------------------------------%

% parse_nn(+NN_Tree, -EntityID, -SideEffects)
%
%	Parse NN subtree and produce an atom representing
%	the main entity of the NN, and also a list of 
%	side effects. 
%	Example: "a black dog" --> entity: dog, side effects: [dog is black]

parse_nn(nn(Noun), UnboundEntityID, [be(UnboundEntityID, Noun)]).

parse_nn(nn(adj(Adjective), NN), X, [property(X, Adjective) | SideEffectsT]):-
	parse_nn(NN, X, SideEffectsT).






% --------------------------------------------------------------------%
% ----------------------        PP        ----------------------------%
% --------------------------------------------------------------------%

% parse_pp(+PP_Tree, ?EntityID, -SemanticRelations)
%
%	Parse PP subtree and produce an atom representing
%	the entity being described, and a list of relations describing it.

parse_pp(pp(prep(at), NP), EntityWithLocation, [location(EntityWithLocation, SemanticID) | SideEffects], []):-
	parse_np(NP, SemanticID, SideEffects, []).

parse_pp(pp(wh(where)), EntityWithLocation, [location(EntityWithLocation, QueriedLocation)], [QueriedLocation]).








being_verb(be).

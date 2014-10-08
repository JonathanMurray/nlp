:- module(semantics, [parse_syntax_tree/3]).


% parse_syntax_tree(+Syntax, -Semantics, -IsQuestion)
%
%	Parse syntax_tree (that represents a sentence) 
%	and produce the semantic meaning. The meaning is
%	a list of relations (facts).
% 	The value of IsQuestion shows if the given sentence
%	is a question (rather than a statement).

parse_syntax_tree(s(NP, VP), Semantics, false):-
	parse_np(NP, Subject, SideEffects),
	parse_vp(VP, Subject, SemanticsVP, _ActionID),
	append(SideEffects, SemanticsVP, Semantics),
	!.
	
parse_syntax_tree(s(S1, cc(_), S2), Semantics, false):-
	parse_syntax_tree(S1, Semantics1, false),
	parse_syntax_tree(S2, Semantics2, false),
	append(Semantics1, Semantics2, Semantics),
	!.

parse_syntax_tree(s(aux(_Aux), NP, VP), Semantics, true):-
	parse_syntax_tree(s(NP, VP), Semantics, _). %Parse as if it were a statement

parse_syntax_tree(s(wh(where), aux(_Aux), NP, VP), [location(ActionID, _Unknown) | Semantics], true):-
	parse_np(NP, Subject, SideEffects),
	parse_vp(VP, Subject, SemanticsVP, ActionID),
	append(SideEffects, SemanticsVP, Semantics).



parse_syntax_tree(question(S), Semantics, true):-
	parse_syntax_tree(S, Semantics, _),
	!.



parse_syntax_tree(SyntaxTree, _, _):-
	write("Couldn't parse syntax tree: "), 
	nl,
	write_compound(SyntaxTree),
	fail.
	



% --------------------------------------------------------------------%
% ----------------------        NP        ----------------------------%
% --------------------------------------------------------------------%

% parse_np(+NP_Tree, -EntityID, -SideEffects)
%
%	Parse NP subtree and produce an atom representing
%	the main entity of the NP, and also a list of 
%	side effects. 
%	Example: "a black dog" --> entity: dog, side effects: [dog is black]


%ind
parse_np(np(nnp(ProperNoun)), ProperNoun, []).

%ind
parse_np(np(pronoun(Pronoun)), Pronoun, []).

%class
parse_np(np(NN), X, SideEffects):-
	parse_nn(NN, X, SideEffects).

%type of dt
parse_np(np(dt(_Determiner), NN), X, SideEffects):-
	parse_nn(NN, X, SideEffects).

%type of nps
parse_np(np(NP1, cc(and), NP2), and(ID1, ID2), SideEffects):-
	parse_np(NP1, ID1, SideEffects1),
	parse_np(NP2, ID2, SideEffects2),
	append(SideEffects1, SideEffects2, SideEffects).

%type of np
parse_np(np(NP, PP), SemanticID, SideEffects):-
	parse_pp(PP, SemanticID, SemanticsPP),
	parse_np(NP, SemanticID, SideEffects2), %This order prevents bad recursion(?)
	append(SemanticsPP, SideEffects2, SideEffects).

%ind
parse_np(np(NP1, s, NP2), PossessionID, [possess(PossesserID, PossessionID) | SideEffectsT]):-
	parse_np(NP2, PossessionID, SideEffectsPossession),
	parse_np(NP1, PossesserID, SideEffectsPossesser),
	append(SideEffectsPossession, SideEffectsPossesser, SideEffectsT).







% --------------------------------------------------------------------%
% ----------------------        NN        ----------------------------%
% --------------------------------------------------------------------%

% parse_nn(+NN_Tree, -EntityID, -SideEffects)
%
%	Parse NN subtree and produce an atom representing
%	the main entity of the NN, and also a list of 
%	side effects. 
%	Example: "a black dog" --> entity: dog, side effects: [dog is black]

parse_nn(nn(Noun), Noun, []).

parse_nn(nn(adj(Adjective), NN), X, [property(X, Adjective) | SideEffectsT]):-
	parse_nn(NN, X, SideEffectsT).








% --------------------------------------------------------------------%
% ----------------------        VP        ----------------------------%
% ---------------------------------------------	----------------------%

% parse_vp(+VP_Tree, -SubjectEntityID, -SemanticMeaning, -ActionID)
%
%	Parse VP subtree and produce 
%	an atom representing the subject of the VP,
%	a list of semantic relations,
%	and an atom representing the action of the VP.



parse_vp(vp(verb(Verb), NP), Subject, [be(Subject, Object) | SideEffects], _):-
	being_verb(Verb),
	parse_np(NP, Object, SideEffects).

parse_vp(vp(verb(Verb), NP), Subject, Semantics, ActionID):-
	not(being_verb(Verb)),
	parse_np(NP, Object, SideEffects),
	generate_action(ActionID, Verb, Subject, Object, SemanticsH),
	append(SemanticsH, SideEffects, Semantics).

parse_vp(vp(verb(Verb), NP, adverb(Adverb)), Subject, Semantics, ActionID):-
	not(being_verb(Verb)),
	parse_np(NP, Object, SideEffects),
	generate_action(ActionID, Verb, Subject, Object, ActionSem),
	append(SideEffects, [property(ActionID, Adverb) | ActionSem], Semantics).

parse_vp(vp(verb(Verb), adverb(Adverb)), Subject, Semantics, ActionID):-
	generate_action(ActionID, Verb, Subject, noone_, ActionSem),
	Semantics = [property(ActionID, Adverb) | ActionSem].

parse_vp(vp(verb(Verb), adj(Adjective)), Subject, [property(Subject, Adjective)], _):-
	being_verb(Verb).

parse_vp(vp(verb(Verb)), Subject, Semantics, ActionID):-
	not(being_verb(Verb)),
	generate_action(ActionID, Verb, Subject, noone_, Semantics),
	!.



parse_vp(vp(vp(verb(Verb)), PP), Subject, Semantics, _):-
	being_verb(Verb),
	parse_pp(PP, Subject, Semantics).


parse_vp(vp(VP, PP), Subject, Semantics, ActionID):-
	parse_vp(VP, Subject, SemanticsVP, ActionID),
	parse_pp(PP, ActionID, SemanticsPP),
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
% ----------------------        PP        ----------------------------%
% --------------------------------------------------------------------%

% parse_pp(+PP_Tree, ?EntityID, -SemanticRelations)
%
%	Parse PP subtree and produce an atom representing
%	the entity being described, and a list of relations describing it.

parse_pp(pp(prep(at), NP), EntityWithLocation, [location(EntityWithLocation, SemanticID) | SideEffects]):-
	parse_np(NP, SemanticID, SideEffects).

parse_pp(pp(prep(with), NP), EntityWithPossession, [possess(EntityWithPossession, SemanticID) | SideEffects]):-
	parse_np(NP, SemanticID, SideEffects).










being_verb(be).


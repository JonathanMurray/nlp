:- module(grammar, [parse_sentence/2]).


% X-argument is the semantic meaning of the input


parse_sentence(SentenceWords, SyntaxTree):-
	sentence(SyntaxTree, SentenceWords, []).


sentence(X) --> base_sentence(X), punctuation_(_).
sentence(X) --> base_sentence(S1), cc_(CoConj), !, sentence(S2), punctuation_(_), {X = s(S1, cc(CoConj), S2)}.
sentence(X) --> base_sentence(S), question_mark(_), {X = question(S)}.

% peter/who met lisa
% peter/who laughs
base_sentence(X) --> np_(NP, _NPType), vp_(VP, _VPType), {X = s(NP, VP)}.

% did peter meet lisa
% did peter laugh
base_sentence(X) --> aux_(Aux), np_(NP, bound), vp_(VP, _VPType), {X = s(aux(Aux), NP, VP)}.

% who did peter meet
base_sentence(X) --> np_(NPObj, query), aux_(Aux), np_(NPSubj, bound), vp_(VP, _VPType), {X = s(NPObj, aux(Aux), NPSubj, VP)}.

%
base_sentence(X) --> np_(NPObj, query), verb_(be, _), np_(NPSub, bound), {X = s(vp(NPObj, verb(be)), NPSub)}.

% where did peter meet lisa
% when did peter laugh
base_sentence(X) --> pp_(PP, query), aux_(Aux), np_(NP, bound), vp_(VP, _VPType), {X = s(PP, aux(Aux), NP, VP)}.

base_sentence(X) --> pp_(PP, query), verb_(be, _), np_(NP, bound), {X = s(PP, vp(verb(be)), NP)}.




%---------------------------------------------
%               NOUN PHRASE
%---------------------------------------------

% Types:
% bound: points at something specific, for instance "peter"
% query: this np has some unbound variables that want to be assigned, for instance "who", or "who's dog"

np_(X, Type) --> np_base(X, Type).
np_(X, bound) --> np_base(NP1, bound), [and], np_base(NP2, bound), {X = np(NP1, cc(and), NP2)}.
np_(X, bound) --> np_base(NP, bound), pp_(PP, bound), {X = np(NP, PP)}.
np_(X, Type) --> np_base(NP1, Type), [s], np_(NP2, bound), {X = np(NP1, s, NP2)}.

np_base(X, bound) --> dt_(DT), !, nn_(NN), {X = np(DT, NN)}.
np_base(X, bound) --> nn_(NN), {X = np(NN)}.
np_base(X, bound) --> nnp_(NNP), {X = np(NNP)}.
np_base(X, bound) --> pronoun_(Pronoun), {X = np(pronoun(Pronoun))}.
np_base(X, query) --> wh_np(Wh), {X = np(wh(Wh))}.

% determiner
dt_(X) --> determiner_(DT), {X = dt(DT)}.

% noun
nn_(X) --> adjective_(Adj), !, nn_(NN), {X = nn(adj(Adj), NN)}.
nn_(X) --> noun_(Noun), {X = nn(Noun)}.

nnp_(X) --> proper_noun_(NNP), {X = nnp(NNP)}.





%---------------------------------------------
%               VERB PHRASE
%---------------------------------------------

% Types:
% trans: transitive, has a second argument (the object)
% intrans: intransitive, only has one argument (the subject)

vp_(X, Type) --> vp_base(X, Type).
vp_(X, Type) --> vp_base(VP, Type), pp_(PP, bound), {X = vp(VP, PP)}.

vp_base(X, intrans) --> verb_(Verb, active), {X = vp(verb(Verb))}.
vp_base(X, trans) --> verb_(Verb, active), np_(NP, _NPType), {X = vp(verb(Verb), NP)}.
vp_base(X, trans) --> verb_(Verb, active), np_(NP, _NPType), adverb_(Adverb), {X = vp(verb(Verb), NP, adverb(Adverb))}.
vp_base(X, intrans) --> verb_(Verb, active), adverb_(Adverb), {X = vp(verb(Verb), adverb(Adverb))}.
vp_base(X, intrans) --> verb_(be, _), adjective_(Adj), {X = vp(verb(be), adj(Adj))}.
vp_base(X, trans) --> verb_(be, _), verb_(call, passive), nnp_(NNP), {X = vp(verb(be), verb(call), NNP)}.





%---------------------------------------------
%               PREPOSITION PHRASE
%---------------------------------------------

% Types:
% bound: for example "in the park"
% query: for example "where"

pp_(X, Type) --> pp_base(X, Type).
pp_(X, Type) --> pp_base(PP, bound), pp_(PP2, Type), {X = pp(PP, PP2)}.
pp_(X, query) --> pp_base(PP, query), pp_(PP2, bound), {X = pp(PP, PP2)}.

pp_base(X, query) --> wh_pp(Wh), {X = pp(wh(Wh))}.
pp_base(X, Type) --> prep_(Prep), np_(NP, Type), {X = pp(prep(Prep), NP)}.






%---------------------------------------------
%                 DICTIONARY
%---------------------------------------------
determiner_(a) --> [a].
determiner_(an) --> [an].
determiner_(the) --> [the].

noun_(dog) --> [dog];[dogs].
noun_(cat) --> [cat];[cats].
noun_(human) --> [human];[person];[people].
noun_(park) --> [park].
noun_(school) --> [school];[university].
noun_(fur) --> [fur].



proper_noun_(peter) --> [peter].
proper_noun_(lisa) --> [lisa].
proper_noun_(johny) --> [johny].

adjective_(cute) --> [cute].
adjective_(little) --> [little];[small];[tiny].
adjective_(big) --> [big];[large];[huge].
adjective_(brown) --> [brown].


adverb_(loud) --> [loudly].


verb_(like, active) --> [love];[loves];[loved];[like];[likes];[liked].
verb_(laugh, active) --> [laugh];[laughs];[laughed].
verb_(possess, active) --> [have];[has];[had];[own];[owns];[owned].
verb_(be, active) --> [be];[is];[am];[are];[were].
verb_(see, active) --> [see];[sees];[saw].
verb_(meet, active) --> [meet];[meets];[met].
verb_(greet, active) --> [greet];[greets];[greeted].
verb_(sleep, active) --> [sleep];[sleeps];[slept].

verb_(call, passive) --> [called].

pronoun_(me) --> [i];[me].
pronoun_(you) --> [you].
pronoun_(he) --> [he].
pronoun_(she) --> [she].
pronoun_(we) --> [we].
pronoun_(it) --> [it].

prep_(at) --> [at];[in];[on];[by].


cc_(and) --> [and].

punctuation_(.) --> [.];[!];[].
question_mark(?) --> [?].

aux_(do) --> [do];[does];[did].
aux_(have) --> [have];[has];[had].


wh_np(who) --> [who].
wh_np(what) --> [what].

wh_pp(where) --> [where].
wh_pp(why) --> [why].
wh_pp(when) --> [when].
wh_pp(how) --> [how].

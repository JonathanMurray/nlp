:- module(grammar, [parse_sentence/2]).

% Based on Penn treebank
% X-argument is the semantic meaning of the input



parse_sentence(SentenceWords, SyntaxTree):-
	sentence(SyntaxTree, SentenceWords, []).


sentence(X) --> base_sentence(X, sentence), punctuation_(_).
sentence(X) --> base_sentence(S1, sentence), cc_(CoConj), !, sentence(S2, sentence), punctuation_(_), {X = s(S1, cc(CoConj), S2)}.
sentence(X) --> base_sentence(S, question), question_mark(_), {X = question(S)}.

base_sentence(X, sentence) --> np_(NP), vp_(VP), {X = s(NP, VP)}.
base_sentence(X, question) --> np_(NP), vp_(VP), {X = s(NP, VP)}.
base_sentence(X, question) --> aux_(Aux), np_(NP), vp_(VP), {X = s(aux(Aux), NP, VP)}.
base_sentence(X, question) --> wh_(Wh), aux_(Aux), np_(NP), vp_(VP), {X = s(wh(Wh), aux(Aux), NP, VP)}.

%---------------------------------------------
%               NOUN PHRASE
%---------------------------------------------

np_(X) --> np_base(X).
np_(X) --> np_base(NP1), [and], np_base(NP2), {X = np(NP1, cc(and), NP2)}.
np_(X) --> np_base(NP), pp_(PP), {X = np(NP, PP)}.
np_(X) --> np_base(NP1), [s], np_(NP2), {X = np(NP1, s, NP2)}.
np_(X) --> np_base(NP1), [s], np_(NP2), {X = np(NP1, s, NP2)}.

np_base(X) --> dt_(DT), !, nn_(NN), {X = np(DT, NN)}.
np_base(X) --> nn_(NN), {X = np(NN)}.
np_base(X) --> nnp_(NNP), {X = np(NNP)}.
np_base(X) --> pronoun_(Pronoun), {X = np(pronoun(Pronoun))}.


% determiner
dt_(X) --> determiner_(DT), {X = dt(DT)}.

% noun
nn_(X) --> adjective_(Adj), !, nn_(NN), {X = nn(adj(Adj), NN)}.
nn_(X) --> noun_(Noun), {X = nn(Noun)}.

nnp_(X) --> proper_noun_(NNP), {X = nnp(NNP)}.



%---------------------------------------------
%               VERB PHRASE
%---------------------------------------------

vp_(X) --> vp_base(X).
vp_(X) --> vp_base(VP), pp_(PP), {X = vp(VP, PP)}.

vp_base(X) --> verb_(Verb), {X = vp(verb(Verb))}.
vp_base(X) --> verb_(Verb), np_(NP), {X = vp(verb(Verb), NP)}.
vp_base(X) --> verb_(Verb), np_(NP), adverb_(Adverb), {X = vp(verb(Verb), NP, adverb(Adverb))}.
vp_base(X) --> verb_(Verb), adverb_(Adverb), {X = vp(verb(Verb), adverb(Adverb))}.
vp_base(X) --> verb_(be), adjective_(Adj), {X = vp(verb(be), adj(Adj))}.


%---------------------------------------------
%               PREPOSITION PHRASE
%---------------------------------------------
pp_(X) --> pp_base(X).
pp_(X) --> pp_base(PP), pp_(PP2), {X = pp(PP, PP2)}.

pp_base(X) --> prep_(Prep), np_(NP), {X = pp(prep(Prep), NP)}.


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
noun_(fur) --> [fur].
%% noun_(_X) --> [what].



proper_noun_(peter) --> [peter].
proper_noun_(lisa) --> [lisa].

adjective_(cute) --> [cute].
adjective_(little) --> [little];[small];[tiny].
adjective_(big) --> [big];[large];[huge].
adjective_(brown) --> [brown].
%% adjective_(_X) --> [what];[how].


adverb_(loud) --> [loudly].


verb_(like) --> [love];[loves];[loved];[like];[likes];[liked].
verb_(laugh) --> [laugh];[laughs];[laughed].
verb_(possess) --> [have];[has];[had];[own];[owns];[owned].
verb_(be) --> [be];[is];[am];[are];[were].
verb_(see) --> [see];[sees];[saw].
verb_(meet) --> [meet];[meets];[met].
verb_(greet) --> [greet];[greets];[greeted].

pronoun_(me) --> [i];[me].
pronoun_(you) --> [you].
pronoun_(he) --> [he].
pronoun_(she) --> [she].
pronoun_(we) --> [we].
pronoun_(it) --> [it].
%% pronoun_(_X) --> [who].

prep_(at) --> [at];[in];[on];[by].
prep_(with) --> [with].


cc_(and) --> [and].

punctuation_(.) --> [.];[!];[].
question_mark(?) --> [?].

aux_(do) --> [do];[does];[did].
aux_(have) --> [have];[has];[had].

wh_(where) --> [where].
wh_(who) --> [who].
wh_(what) --> [what].
wh_(why) --> [why].
wh_(when) --> [when].
wh_(how) --> [how].

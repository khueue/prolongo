:- module(_,
    [
        encode/2
    ]).

% <module> BSON encoder.

:- use_module(bson_bits, []).

:- include(misc(common)).

%%  encode(+Term, ?Bson:list) is semidet.
%
%   True if xxx.

encode(Term, Bson) :-
    Bson = [Length|_BsonElements],
    phrase(elements(Term,Length), Bson, [0]),
    !.
encode(_Term, _Bson) :-
    throw(bson_error(invalid)).

elements([], 0) --> !, [].
elements([key:value|Elements], Length) -->
    [Length,k,e,y,v,a,l,u,e],
    elements(Elements, Length0),
    {Length is Length0 + 8}.

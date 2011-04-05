:- include(misc(common)).

:- begin_tests('bson_encoder:encode/2').

test('empty doc', [true(Got == Expected)]) :-
    Term =
    [
        key:value
    ],
    Expected =
    [
        8,
        k,e,y,v,a,l,u,e,
        0
    ],
    bson_encoder:encode(Term, Got).

:- end_tests('bson_encoder:encode/2').

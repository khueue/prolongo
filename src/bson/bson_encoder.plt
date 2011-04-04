:- include(misc(common)).

:- begin_tests('bson_encoder:encode/2').

test('xxx', [true(Got == Expected)]) :-
    Expected = xxx,
    bson_encoder:encode(xxx, Got).

:- end_tests('bson_encoder:encode/2').

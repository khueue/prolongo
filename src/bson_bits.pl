:- module(_, []).

:-
    absolute_file_name('bson_bits', [relative_to('./lib')], LibPath),
    use_foreign_library(foreign(LibPath)).

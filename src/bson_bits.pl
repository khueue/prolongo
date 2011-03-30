:- module(bson_bits, [bytes_as_float/8]).
:-
    absolute_file_name('bson_bits', [relative_to('./lib')], LibPath),
    use_foreign_library(foreign(LibPath)).

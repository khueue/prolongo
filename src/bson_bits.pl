:- module(bson_bits, [bytes_to_float/9,bytes_to_integer/5]).
:-
    absolute_file_name('bson_bits', [relative_to('./lib')], LibPath),
    use_foreign_library(foreign(LibPath)).

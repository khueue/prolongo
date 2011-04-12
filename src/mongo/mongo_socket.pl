:- module(mongo_socket,
    [
        create_client/0
    ]).

% <module> Trying some sockets.

:- include(misc(common)).

mongodb_default_port(27017).

create_client :-
    Host = '0.0.0.0',
    mongodb_default_port(Port),
    setup_call_catcher_cleanup(
        socket:tcp_socket(Socket),
        socket:tcp_connect(Socket, Host:Port),
        exception(_),
        socket:tcp_close_socket(Socket)),
    setup_call_cleanup(
        socket:tcp_open_socket(Socket, Read, Write),
        chat_to_server(Read, Write),
        close_connection(Read, Write)).

close_connection(Read, Write) :-
    core:close(Read, [force(true)]),
    core:close(Write, [force(true)]).

chat_to_server(Read, Write) :-
    core:read(Term),
    process_request(Term, Read, Write).

process_request(end_of_file, _Read, _Write) :- !.
process_request(Term, Read, Write) :-
    core:format(Write, '~q.~n', [Term]),
    core:flush_output(Write),
    core:read(Read, Reply),
    core:format('Reply: ~q.~n', [Reply]),
    chat_to_server(Read, Write).

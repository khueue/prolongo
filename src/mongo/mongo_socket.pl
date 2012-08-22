:- module(mongo_socket,
    [
        new_socket/3,
        free_socket/1,
        send_bytes/2,
        receive_n_bytes/3
    ]).

/** <module> Low-level socket handling.
 */

:- include(misc(common)).

:- use_module(misc(util), []).

%%  new_socket.
%
%   xxxxxxx

new_socket(Host, Port, Socket) :-
    setup_call_catcher_cleanup(
        socket:tcp_socket(SocketId),
        socket:tcp_connect(SocketId, Host:Port, ReadStream, WriteStream),
        exception(_),
        close_socket_and_throw(SocketId)),
    Socket = socket(ReadStream,WriteStream).

close_socket_and_throw(SocketId) :-
    socket:tcp_close_socket(SocketId),
    throw(mongo_error('could not connect to server', [])).

socket_read(Socket, ReadStream) :-
    util:get_arg(Socket, 1, ReadStream).

socket_write(Socket, WriteStream) :-
    util:get_arg(Socket, 2, WriteStream).

%%  free_socket.
%
%   xxxxxxx

free_socket(Socket) :-
    socket_read(Socket, ReadStream),
    socket_write(Socket, WriteStream),
    core:close(ReadStream, [force(true)]),
    core:close(WriteStream, [force(true)]).

%%  send_bytes.
%
%   xxxxxxxxx

send_bytes(Socket, Bytes) :-
    socket_write(Socket, WriteStream),
    send_bytes_and_flush(Bytes, WriteStream).

send_bytes_and_flush(Bytes, WriteStream) :-
    core:format(WriteStream, '~s', [Bytes]),
    core:flush_output(WriteStream).

%%  receive_n_bytes.
%
%   xxxxxxxx

receive_n_bytes(Socket, N, Bytes) :-
    socket_read(Socket, ReadStream),
    receive_n_bytes_aux(ReadStream, N, Bytes).

receive_n_bytes_aux(_ReadStream, 0, []) :- !.
receive_n_bytes_aux(ReadStream, N, [Byte|Bytes]) :-
    core:get_byte(ReadStream, Byte),
    N1 is N - 1,
    receive_n_bytes_aux(ReadStream, N1, Bytes).

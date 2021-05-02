%use with :- use_module(server).
%or include raw with :- include(server.pl).


:- module(echo_server,
  [ start_server/0,
    stop_server/0
  ]
).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/websocket)).


:- http_handler(root(.),
                http_reply_from_files('.', []),
                [prefix]).

:- http_handler(root(echo),
                http_upgrade_to_websocket(echo, []),
                [spawn([])]).

start_server :-
    default_port(Port),
    start_server(Port).
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

stop_server() :-
    default_port(Port),
    stop_server(Port).
stop_server(Port) :-
    http_stop_server(Port, []).

default_port(3000).


echo(WebSocket) :-
    ws_receive(WebSocket, Message, [format(json)]),
    ( Message.opcode == close
    -> true
    ; get_response(Message.data, Response),
      write("Response: "), writeln(Response),
      ws_send(WebSocket, json(Response)),
      echo(WebSocket)
    ).


get_response(Message, Response) :-
  get_time(Time),
  Response = _{message:Message.message, time: Time}.

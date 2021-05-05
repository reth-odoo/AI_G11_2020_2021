%use with :- use_module(server).
%or include raw with :- include(server.pl).

/*imports*/
:- module(echo_server,
  [ start_server/0,
    stop_server/0
  ]
).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/websocket)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).

/*parameters*/

default_port(3000).

/*start*/
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

/*handlers*/
:- http_handler(root(.),
                http_reply_from_files('.', []),
                [prefix]).

:- http_handler(root(quoridor/AI),
                quoridorai_handler,
                [spawn([])]).

:- http_handler(root(quoridor/CheckMove),
                quoridormv_handler,
                [spawn([])]).

:- http_handler(root(quoridor/CheckWall),
                quoridorwl_handler,
                [spawn([])]).

:- http_handler(root(chat),
                char_handler,
                [spawn([])]).


quoridorai_handler(Req):-state_from_req(Req, X),next_ai_move(X),reply_json_dict(Out).
quoridorwl_handler(Req):-state_from_req(Req, X),is_valid_move(X),reply_json_dict(Out).
quoridormv_handler(Req):-state_from_req(Req, X),can_place_wall(X),reply_json_dict(Out).

chat_handler(Req):-http_read_json_dict(Req, Dict), respond_to_dict
/*handler utility*/
state_from_req(Req,X):-http_read_json_dict(Req, Dict), parse_quoridor_state(Dict, X).

/*parsers*/

parse_quoridor_state(State, State.pawns, State.walls, State.wall_numbers, State.player_number).
parse_quorido_command()

json_true_false(Bool,JsonBool) :- (Bool,JsonBool = _{can: true});(not(Bool),JsonBool = _{can: false}).
json_state(PawnPositions, WallPositions, WallNB, JsonState):- JsonState = _{pawns: PawnPositions, walls: WallPositions, wall_numbers: WallNB}.

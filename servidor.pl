%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Servidor em prolog

% Módulos:
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dirindex)).
%DEBUG:
%:- use_module(library(http/http_error)).
%:- debug.

% GET
:- http_handler(
    root(action), % Alias /action
    action,       % Predicado 'action'
    []).

:- http_handler(root(.), http_reply_from_files('.', []), [prefix]).

:- json_object
    controles(forward:integer, reverse: integer, left:integer, right:integer).

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

stop_server(Port) :-
    http_stop_server(Port, []).

action(Request) :-
    http_parameters(Request,
                    % sensores do carro:
                    [ x(X, [float]),
                      y(Y, [float]),
                      angle(ANGLE, [float]),
                      s1(S1, [float]),
                      s2(S2, [float]),
                      s3(S3, [float]),
                      s4(S4, [float]),
                      s5(S5, [float])
                    ]),
    SENSORES = [X,Y,ANGLE,S1,S2,S3,S4,S5],
    obter_controles(SENSORES, MELHOR),
    CONTROLES = [FORWARD, REVERSE, LEFT, RIGHT],
    prolog_to_json( controles(FORWARD, REVERSE, LEFT, RIGHT), JOut ),
    reply_json( JOut ).

start :- format('~n~n--========================================--~n~n'),
         start_server(8080),
         format('~n~n--========================================--~n~n').
:- initialization start.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

acoes([X,Y,ANGLE,S1,S2,S3,S4,S5], [0,1,0,0]):-S3 > 0.9, S2 > 0.9, S4 < 0.9.
acoes([X,Y,ANGLE,S1,S2,S3,S4,S5], [0,0,1,0]):-S4 > 0.5. 
acoes([X,Y,ANGLE,S1,S2,S3,S4,S5], [0,0,0,1]):-S1 > 0.5.
acoes([X,Y,ANGLE,S1,S2,S3,S4,S5], [1,0,1,0]):-S4+S5 > S1+S2+0.1.
acoes([X,Y,ANGLE,S1,S2,S3,S4,S5], [1,0,0,1]):-S1+S2 > S4+S5+0.1.
acoes([X,Y,ANGLE,S1,S2,S3,S4,S5], [1,0,0,1]):-ANGLE =< -1.
acoes([X,Y,ANGLE,S1,S2,S3,S4,S5], [1,0,0,1]):-ANGLE >= 1.
acoes([X,Y,ANGLE,S1,S2,S3,S4,S5], [1,0,0,0]):-S3 < 0.3.
acoes([X,Y,ANGLE,S1,S2,S3,S4,S5], [1,0,0,0]):-S2 < 0.7, S3 < 0.5, S4 < 0.7, ANGLE > -0.1, ANGLE < 0.1.


todasAcoes(SENSORES,AUX, LIST) :-
    acoes(SENSORES, X),
    \+member(X, AUX),
    todasAcoes(SENSORES,[X|AUX], LIST).
todasAcoes(SENSORES,L, L).  

avaliar(SENSORES, [F,R,E,D],P):- P is F*20+ R*5 + E*10 + D*10.

melhorAcao([], _, MELHOR, MELHOR):- !.
melhorAcao([H|T], MAIOR,_,MELHOR):-
    avaliar(SENSORES, MELHOR,N),
    N > MAIOR,
    melhorAcao(T,N, H, MELHOR).
melhorAcao([_|T],MAIOR, MELHORAUX, MELHOR):-
    melhorAcao(T,MAIOR, MELHORAUX, MELHOR).

obter_controles(SENSORES, MELHOR) :- todasAcoes(SENSORES, [], T), melhorAcao(T, -99999, [0,0,0,0], MELHOR),  ACAO=MELHOR.
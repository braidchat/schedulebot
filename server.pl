:- module(server, []).
:- use_module(transit).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/thread_httpd)).


run(Port) :- http_server(http_dispatch, [port(Port)]).

:- http_handler(root(.), braid_msg_handler, []).

braid_msg_handler(Request) :-
    % body as string
    % read transit
    % verify signature
    true.

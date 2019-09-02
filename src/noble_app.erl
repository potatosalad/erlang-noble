%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2019, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  31 August 2019 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(noble_app).

-behaviour(application).

%% application callbacks
-export([start/2]).
-export([stop/1]).

%%%===================================================================
%%% application callbacks
%%%===================================================================

start(_Type, _Args) ->
	noble_sup:start_link().

stop(_State) ->
	ok.

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
-module(noble_schema).

%% Public API
-export([setup/0]).
-export([try_load_command/0]).
-export([try_load_event/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

setup() ->
	{ok, CommandSchema} = try_load_command(),
	{ok, EventSchema} = try_load_event(),
	true = ets:insert(?MODULE, {command, CommandSchema}),
	true = ets:insert(?MODULE, {event, EventSchema}),
	ok.

try_load_command() ->
	case file:read_file(filename:join([noble:priv_dir(), "node_modules", "noble-ipc", "command.schema.json"])) of
		{ok, Content} ->
			JSON = 'Elixir.Jason':'decode!'(Content),
			case 'Elixir.JsonXema':'new'(JSON) of
				#{schema := #{definitions := #{<<"Command">> := Schema}}} ->
					{ok, Schema}
			end
	end.

try_load_event() ->
	case file:read_file(filename:join([noble:priv_dir(), "node_modules", "noble-ipc", "event.schema.json"])) of
		{ok, Content} ->
			JSON = 'Elixir.Jason':'decode!'(Content),
			case 'Elixir.JsonXema':'new'(JSON) of
				#{schema := #{definitions := #{<<"Event">> := Schema}}} ->
					{ok, Schema}
			end
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

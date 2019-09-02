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
-module(noble_sup).

-behaviour(supervisor).

%% Public API
-export([start_link/0]).
%% supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([]) ->
	noble_schema = ets:new(noble_schema, [named_table, public, {read_concurrency, true}, set]),
	ok = noble_schema:setup(),
	ChildSpecs = [
		#{
			id => noble_port_sup,
			start => {noble_port_sup, start_link, []},
			restart => permanent,
			type => supervisor,
			shutdown => 5000
		}
	],
	SupFlags = #{
		strategy => one_for_one,
		intensity => 1,
		period => 5
	},
	{ok, {SupFlags, ChildSpecs}}.

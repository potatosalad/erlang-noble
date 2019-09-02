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
-module(noble_port_sup).

-behaviour(supervisor).

%% Public API
-export([start_link/0]).
-export([start_child/1]).
%% supervisor callbacks
-export([init/1]).

%% Macros
% -define(STRATEGY, one_for_one).
-define(STRATEGY, simple_one_for_one).

%%%===================================================================
%%% Public API
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, ?STRATEGY).

start_child(Owner) when is_pid(Owner) ->
	case ?STRATEGY of
		one_for_one ->
			supervisor:start_child(?MODULE, #{
				id => {noble_port, Owner},
				start => {noble_port, start_link, [Owner]},
				restart => transient,
				type => worker,
				shutdown => 5000
			});
		simple_one_for_one ->
			supervisor:start_child(?MODULE, [Owner])
	end.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init(one_for_one) ->
	ChildSpecs = [],
	SupFlags = #{
		strategy => one_for_one,
		intensity => 1,
		period => 5
	},
	{ok, {SupFlags, ChildSpecs}};
init(simple_one_for_one) ->
	ChildSpecs = [
		#{
			id => noble_port,
			start => {noble_port, start_link, []},
			restart => temporary,
			type => worker,
			shutdown => brutal_kill
		}
	],
	SupFlags = #{
		strategy => simple_one_for_one,
		intensity => 0,
		period => 1
	},
	{ok, {SupFlags, ChildSpecs}}.

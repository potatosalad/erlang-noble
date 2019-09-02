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
-module(noble_port).

-behaviour(gen_statem).

%% Public API
-export([open/0]).
-export([close/1]).
-export([controlling_process/2]).
-export([port_command/2]).
-export([port_connect/2]).
-export([port_getopt/2]).
-export([port_info/2]).
-export([port_setopt/3]).
%% Internal API
-export([start_link/1]).
%% gen_statem callbacks
-export([callback_mode/0]).
-export([init/1]).
-export([handle_event/4]).
-export([terminate/3]).

%% Records
-record(data, {
	active = true :: false | true | once,
	buffer = <<>> :: binary(),
	closer = undefined :: undefined | {pid(), reference()},
	latency = 0 :: non_neg_integer(),
	monitor = undefined :: undefined | reference(),
	owner = undefined :: undefined | pid(),
	pattern = undefined :: undefined | binary:cp(),
	ping = undefined :: undefined | {binary(), integer()},
	port = undefined :: undefined | port(),
	queue = queue:new() :: queue:queue(term())
}).

%%%===================================================================
%%% Public API
%%%===================================================================

open() ->
	open(self()).

open(Owner) when is_pid(Owner) ->
	noble_port_sup:start_child(Owner).

close(Port) ->
	case erlang:is_process_alive(Port) of
		false ->
			ok;
		true ->
			case controlling_process(Port, self()) of
				ok ->
					receive
						{noble_port_closed, Port} ->
							catch gen_statem:call(Port, close, 5000),
							self() ! {noble_port_closed, Port},
							ok
					after
						0 ->
							catch gen_statem:call(Port, close, 5000),
							ok
					end;
				ControlError ->
					ControlError
			end
	end.

controlling_process(Port, NewOwner) when is_pid(NewOwner) ->
	case ?MODULE:port_info(Port, connected) of
		{connected, NewOwner} ->
			ok;
		{connected, Pid} when Pid =/= self() ->
			{error, not_owner};
		undefined ->
			{error, einval};
		_ ->
			case ?MODULE:port_getopt(Port, active) of
				{ok, A0} ->
					SetOptRes =
						case A0 of
							false -> ok;
							_ -> ?MODULE:port_setopt(Port, active, false)
						end,
					case {sync_input(Port, NewOwner, false), SetOptRes} of
						{true, _} -> %% already closed
							ok;
						{false, ok} ->
							try ?MODULE:port_connect(Port, NewOwner) of
								true ->
									case A0 of
										false -> ok;
										_ -> ?MODULE:port_setopt(Port, active, A0)
									end
							catch
								error:Reason ->
									{error, Reason}
							end;
						{false, Error} ->
							Error
					end;
				Error ->
					Error
			end
	end.

port_command(Port, Command = #{<<"action">> := _}) ->
	case controlling_process(Port, self()) of
		ok ->
			Data = noble_command:encode(Command),
			ok = gen_statem:cast(Port, {self(), {command, Data}}),
			true;
		ControlError ->
			ControlError
	end.

port_connect(Port, Pid) when is_pid(Pid) ->
	case erlang:is_process_alive(Port) of
		false ->
			true;
		true ->
			gen_statem:call(Port, {self(), {connect, Pid}}, 5000)
	end;
port_connect(Port, BadPid) ->
	erlang:error({badarg, [Port, BadPid]}).

port_getopt(Port, active) ->
	case erlang:is_process_alive(Port) of
		false ->
			{error, einval};
		true ->
			gen_statem:call(Port, {port_getopt, active}, 5000)
	end;
port_getopt(Port, BadKey) ->
	erlang:error({badarg, [Port, BadKey]}).

port_info(Port, Key)
		when Key =:= connected
		orelse Key =:= latency ->
	case erlang:is_process_alive(Port) of
		false ->
			undefined;
		true ->
			gen_statem:call(Port, {port_info, Key}, 5000)
	end;
port_info(Port, BadKey) ->
	erlang:error({badarg, [Port, BadKey]}).

port_setopt(Port, active, Active)
		when Active =:= false
		orelse Active =:= once
		orelse Active =:= true ->
	case controlling_process(Port, self()) of
		ok ->
			try gen_statem:call(Port, {self(), {setopt, active, Active}}, 5000) of
				ok ->
					ok;
				SetoptsError ->
					SetoptsError
			catch
				error:Error ->
					_ = close(Port),
					receive
						{noble_port_closed, Port} ->
							{error, Error}
					after
						0 ->
							{error, Error}
					end
			end;
		ControlError ->
			ControlError
	end;
port_setopt(Port, BadKey, BadVal) ->
	erlang:error({badarg, [Port, BadKey, BadVal]}).

%%%===================================================================
%%% Internal API
%%%===================================================================

start_link(Owner) when is_pid(Owner) ->
	gen_statem:start_link(?MODULE, {Owner}, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
	[handle_event_function, state_enter].

init({Owner}) when is_pid(Owner) ->
	Pattern = binary:compile_pattern([<<"\r\n">>, <<"\r">>, <<"\n">>]),
	Monitor = erlang:monitor(process, Owner),
	Data = #data{monitor=Monitor, owner=Owner, pattern=Pattern},
	{ok, init, Data}.

%% State Enter Events
handle_event(enter, init, init, _Data=#data{port=undefined}) ->
	Actions = [{state_timeout, 0, open_port}],
	{keep_state_and_data, Actions};
handle_event(enter, init, open, _Data) ->
	Actions = [{state_timeout, 0, ping}],
	{keep_state_and_data, Actions};
handle_event(enter, _OldState, closed, Data0) ->
	Data1 = shutdown(Data0),
	Actions = [{state_timeout, 0, maybe_stop}],
	{keep_state, Data1, Actions};
%% State Timeout Events
handle_event(state_timeout, open_port, init, Data=#data{port=undefined}) ->
	Script = filename:join([noble:priv_dir(), "noble-ipc-server-stdio.sh"]),
	Port = erlang:open_port({spawn_executable, Script}, [
		binary,
		exit_status,
		stderr_to_stdout,
		stream,
		use_stdio
	]),
	{next_state, open, Data#data{port=Port}};
handle_event(state_timeout, ping, open, Data=#data{ping=undefined, port=Port}) when Port =/= undefined ->
	PingData = crypto:strong_rand_bytes(8),
	PingCommand = noble_command:ping(PingData),
	Command = noble_command:encode(PingCommand),
	Ping = {PingData, erlang:monotonic_time(microsecond)},
	true = erlang:port_command(Port, [Command, $\n]),
	Actions = [{state_timeout, 5000, pong}],
	{keep_state, Data#data{ping=Ping}, Actions};
handle_event(state_timeout, pong, open, Data0=#data{ping={PingData, StartTime}, port=Port}) when Port =/= undefined ->
	StopTime = erlang:monotonic_time(microsecond),
	{ok, Data1} = send_to_owner({noble_port_error, self(), {pong_timeout, PingData, StartTime, StopTime}}, Data0),
	{next_state, closed, Data1};
handle_event(state_timeout, maybe_stop, closed, Data) ->
	case Data of
		#data{owner=undefined} ->
			{stop, normal, Data};
		#data{monitor=Monitor, queue=Queue} ->
			case queue:is_empty(Queue) of
				false ->
					keep_state_and_data;
				true ->
					_ = erlang:demonitor(Monitor, [flush]),
					{stop, normal, Data#data{monitor=undefined, owner=undefined}}
			end
	end;
%% Call Events (read)
handle_event({call, From}, {port_info, connected}, _State, _Data=#data{owner=Owner}) ->
	Actions = [{reply, From, {connected, Owner}}],
	{keep_state_and_data, Actions};
handle_event({call, From}, {port_info, latency}, _State, _Data=#data{latency=Latency}) ->
	Actions = [{reply, From, {latency, Latency}}],
	{keep_state_and_data, Actions};
handle_event({call, From}, {port_getopt, active}, _State, _Data=#data{active=Active}) ->
	Actions = [{reply, From, {ok, Active}}],
	{keep_state_and_data, Actions};
%% Call Events (write: owner)
handle_event({call, From}, {Owner, close}, _State, Data=#data{closer=undefined, owner=Owner}) ->
	{next_state, closed, Data#data{closer=From}};
handle_event({call, From}, {Owner, {connect, NewOwner}}, _State, Data=#data{monitor=OldMonitor, owner=Owner}) when is_pid(NewOwner) ->
	_ = erlang:demonitor(OldMonitor, [flush]),
	NewMonitor = erlang:monitor(process, NewOwner),
	Actions = [{reply, From, true}],
	{keep_state, Data#data{monitor=NewMonitor, owner=NewOwner}, Actions};
handle_event({call, From}, {Owner, {setopt, active, Active}}, State, Data0=#data{owner=Owner}) ->
	{Reply, Data1} = setopt_active(Active, Data0),
	Actions = case State of
		closed -> [{reply, From, Reply}, {state_timeout, 0, maybe_stop}];
		_ -> [{reply, From, Reply}]
	end,
	{keep_state, Data1, Actions};
%% Call Events (write: not owner)
handle_event({call, From}, {_Owner, close}, _State, _Data) ->
	Actions = [{reply, From, {error, not_owner}}],
	{keep_state_and_data, Actions};
handle_event({call, From}, {_Owner, {connect, _NewOwner}}, _State, _Data) ->
	Actions = [{reply, From, {error, not_owner}}],
	{keep_state_and_data, Actions};
handle_event({call, From}, {_Owner, {setopt, active, _Active}}, _State, _Data) ->
	Actions = [{reply, From, {error, not_owner}}],
	{keep_state_and_data, Actions};
%% Cast Events
handle_event(cast, {Owner, {command, Command}}, open, _Data=#data{owner=Owner, port=Port}) when is_binary(Command) ->
	true = erlang:port_command(Port, [Command, $\n]),
	keep_state_and_data;
handle_event(cast, {_Owner, {command, _Command}}, _State, _Data) ->
	keep_state_and_data;
%% Info Events
handle_event(info, {Port, {data, PortData}}, State = open, Data0=#data{buffer=SoFar, pattern=Pattern, port=Port}) ->
	case binary:split(PortData, Pattern) of
		[_] ->
			{Actions, Data1} = maybe_ping(Data0#data{buffer = <<SoFar/binary, PortData/binary>>}),
			{keep_state, Data1, Actions};
		[Tail, Rest] ->
			Event = noble_event:decode(<<SoFar/binary, Tail/binary>>),
			ok = noble_event:validate(Event),
			{ok, Data1} = maybe_pong(Event, Data0#data{buffer = <<>>}),
			{ok, Data2} = send_to_owner({noble_port_data, self(), Event}, Data1),
			case Event of
				#{<<"type">> := <<"stop">>} ->
					{next_state, closed, Data2};
				_ ->
					handle_event(info, {Port, {data, Rest}}, State, Data2)
			end
	end;
handle_event(info, {'DOWN', Monitor, process, Owner, _Reason}, State, Data=#data{monitor=Monitor, owner=Owner}) when is_reference(Monitor) andalso is_pid(Owner) ->
	case State of
		closed ->
			Actions = [{state_timeout, 0, maybe_stop}],
			{keep_state, Data#data{monitor=undefined, owner=undefined}, Actions};
		_ ->
			{next_state, closed, Data#data{monitor=undefined, owner=undefined}}
	end;
handle_event(info, {Port, {exit_status, ExitStatus}}, State, Data0=#data{port=Port}) when Port =/= undefined ->
	case State of
		closed ->
			Actions = [{state_timeout, 0, maybe_stop}],
			{keep_state, Data0#data{port=undefined}, Actions};
		_ ->
			{ok, Data1} = send_to_owner({noble_port_error, self(), {exit_status, ExitStatus}}, Data0),
			{next_state, closed, Data1#data{port=undefined}}
	end.

terminate(_Reason, _State, _Data=#data{closer=undefined, owner=undefined, port=undefined}) ->
	ok;
terminate(Reason, State, Data=#data{closer=From, owner=undefined, port=undefined}) ->
	catch gen_statem:reply(From, ok),
	terminate(Reason, State, Data#data{closer=undefined});
terminate(Reason, State, Data=#data{monitor=Monitor, owner=Owner, port=undefined}) when is_pid(Owner) ->
	catch erlang:send(Owner, {noble_port_closed, self()}),
	_ = erlang:demonitor(Monitor, [flush]),
	terminate(Reason, State, Data#data{monitor=undefined, owner=undefined});
terminate(Reason, State, Data=#data{port=Port}) when Port =/= undefined ->
	true = erlang:port_close(Port),
	terminate(Reason, State, Data#data{port=undefined}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
flush_to_owner(Data=#data{active=false}) ->
	{ok, Data};
flush_to_owner(Data=#data{active=once, queue=Q0, owner=Owner}) ->
	case queue:out(Q0) of
		{{value, Message}, Q1} ->
			catch erlang:send(Owner, Message),
			flush_to_owner(Data#data{active=false, queue=Q1});
		{empty, Q0} ->
			{ok, Data}
	end;
flush_to_owner(Data=#data{active=true, queue=Q0, owner=Owner}) ->
	case queue:out(Q0) of
		{{value, Message}, Q1} ->
			catch erlang:send(Owner, Message),
			flush_to_owner(Data#data{queue=Q1});
		{empty, Q0} ->
			{ok, Data}
	end.

%% @private
maybe_ping(Data=#data{ping=undefined}) ->
	Actions = [{state_timeout, 5000, ping}],
	{Actions, Data};
maybe_ping(Data) ->
	Actions = [],
	{Actions, Data}.

%% @private
maybe_pong(#{<<"type">> := <<"pong">>, <<"data">> := PingData}, Data=#data{latency=Latency0, ping={PingData, StartTime}}) ->
	StopTime = erlang:monotonic_time(microsecond),
	DiffTime = StopTime - StartTime,
	Latency1 =
		case Latency0 of
			0 -> DiffTime;
			_ -> (Latency0 + DiffTime) div 2
		end,
	{ok, Data#data{latency=Latency1, ping=undefined}};
maybe_pong(_Event, Data) ->
	{ok, Data}.

%% @private
send_to_owner(_Message, Data=#data{owner=undefined}) ->
	{ok, Data};
send_to_owner(Message, Data0=#data{active=true, owner=Owner}) ->
	{ok, Data1} = flush_to_owner(Data0),
	catch erlang:send(Owner, Message),
	{ok, Data1};
send_to_owner(Message, Data0=#data{active=once, owner=Owner}) ->
	case flush_to_owner(Data0) of
		{ok, Data0} ->
			catch erlang:send(Owner, Message),
			{ok, Data0#data{active=false}};
		{ok, Data1=#data{queue=Q0}} ->
			Q1 = queue:in(Message, Q0),
			{ok, Data1#data{queue=Q1}}
	end;
send_to_owner(Message, Data=#data{active=false, queue=Queue}) ->
	{ok, Data#data{queue=queue:in(Message, Queue)}}.

%% @private
setopt_active(Active, Data0)
		when Active =:= false
		orelse Active =:= once
		orelse Active =:= true ->
	{ok, Data1} = flush_to_owner(Data0#data{active=Active}),
	{ok, Data1};
setopt_active(_Active, Data) ->
	{{error, enotsup}, Data}.

%% @private
shutdown(Data=#data{owner=undefined, port=undefined}) ->
	Data;
shutdown(Data0=#data{port=undefined}) ->
	{ok, Data1} = send_to_owner({noble_port_closed, self()}, Data0),
	Data1;
shutdown(Data=#data{port=Port}) when Port =/= undefined ->
	true = erlang:port_close(Port),
	shutdown(Data#data{port=undefined}).

%% @private
sync_input(P, Owner, Flag) ->
	receive
		{noble_port_data, P, Data} ->
			Owner ! {noble_port_data, P, Data},
			sync_input(P, Owner, Flag);
		{noble_port_error, P, Error} ->
			Owner ! {noble_port_error, P, Error},
			sync_input(P, Owner, Flag);
		{noble_port_closed, P} ->
			Owner ! {noble_port_closed, P},
			sync_input(P, Owner, true)
	after
		0 ->
			Flag
	end.

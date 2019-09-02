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
-module(noble_command).

%% Public API
-export([decode/1]).
-export([encode/1]).
-export([schema/1]).
-export([validate/1]).
%% Command API
-export([start_scanning/0]).
-export([start_scanning/1]).
-export([start_scanning/2]).
-export([stop_scanning/0]).
-export([connect/1]).
-export([disconnect/1]).
-export([update_rssi/1]).
-export([discover_services/2]).
-export([discover_included_services/3]).
-export([discover_characteristics/3]).
-export([read/3]).
-export([write/5]).
-export([broadcast/4]).
-export([notify/4]).
-export([discover_descriptors/3]).
-export([read_value/4]).
-export([write_value/5]).
-export([read_handle/2]).
-export([write_handle/4]).
-export([ping/1]).
-export([stop/0]).

%% Macros
-define(UC(C),
	case C of
		$a -> $A;
		$b -> $B;
		$c -> $C;
		$d -> $D;
		$e -> $E;
		$f -> $F;
		$g -> $G;
		$h -> $H;
		$i -> $I;
		$j -> $J;
		$k -> $K;
		$l -> $L;
		$m -> $M;
		$n -> $N;
		$o -> $O;
		$p -> $P;
		$q -> $Q;
		$r -> $R;
		$s -> $S;
		$t -> $T;
		$u -> $U;
		$v -> $V;
		$w -> $W;
		$x -> $X;
		$y -> $Y;
		$z -> $Z;
		_ -> C
	end).

%%%===================================================================
%%% Public API
%%%===================================================================

decode(Input) when is_binary(Input) ->
	maybe_decode_command('Elixir.Jason':'decode!'(Input)).

encode(Command = #{<<"action">> := _}) ->
	'Elixir.Jason':'encode!'(maybe_encode_command(Command)).

schema(#{<<"action">> := Action}) ->
	schema(Action);
schema(<< C, Rest/binary >>) ->
	Action = << (?UC(C)), Rest/binary >>,
	Definitions = ets:lookup_element(noble_schema, command, 2),
	maps:find(Action, Definitions);
schema(_) ->
	error.

validate(Command = #{<<"action">> := _}) ->
	{ok, CommandSchema} = schema(<<"Command">>),
	case 'Elixir.JsonXema':'validate'(CommandSchema, Command) of
		ok ->
			ok;
		EventSchemaError ->
			%% Try to validate against a more specific version of the command schema.
			case schema(Command) of
				{ok, ActionSchema} ->
					case 'Elixir.JsonXema':'validate'(ActionSchema, Command) of
						ok ->
							EventSchemaError;
						ActionSchemaError ->
							ActionSchemaError
					end;
				error ->
					EventSchemaError
			end
	end.

%%%===================================================================
%%% Command API
%%%===================================================================

start_scanning() ->
	#{
		<<"action">> => <<"startScanning">>
	}.

start_scanning(ServiceUuids)
		when is_list(ServiceUuids) ->
	#{
		<<"action">> => <<"startScanning">>,
		<<"serviceUuids">> => ServiceUuids
	};
start_scanning(AllowDuplicates)
		when is_boolean(AllowDuplicates) ->
	#{
		<<"action">> => <<"startScanning">>,
		<<"allowDuplicates">> => AllowDuplicates
	}.

start_scanning(ServiceUuids, AllowDuplicates)
		when is_list(ServiceUuids)
		andalso is_boolean(AllowDuplicates) ->
	#{
		<<"action">> => <<"startScanning">>,
		<<"serviceUuids">> => ServiceUuids,
		<<"allowDuplicates">> => AllowDuplicates
	}.

stop_scanning() ->
	#{
		<<"action">> => <<"stopScanning">>
	}.

connect(PeripheralUuid)
		when is_binary(PeripheralUuid) ->
	#{
		<<"action">> => <<"connect">>,
		<<"peripheralUuid">> => PeripheralUuid
	}.

disconnect(PeripheralUuid)
		when is_binary(PeripheralUuid) ->
	#{
		<<"action">> => <<"disconnect">>,
		<<"peripheralUuid">> => PeripheralUuid
	}.

update_rssi(PeripheralUuid)
		when is_binary(PeripheralUuid) ->
    #{
        <<"action">> => <<"updateRssi">>,
        <<"peripheralUuid">> => PeripheralUuid
    }.

discover_services(PeripheralUuid, ServiceUuids)
        when is_binary(PeripheralUuid)
        andalso is_list(ServiceUuids) ->
    #{
        <<"action">> => <<"discoverServices">>,
        <<"peripheralUuid">> => PeripheralUuid,
        <<"serviceUuids">> => ServiceUuids
    }.

discover_included_services(PeripheralUuid, ServiceUuid, ServiceUuids)
        when is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_list(ServiceUuids) ->
    #{
        <<"action">> => <<"discoverIncludedServices">>,
        <<"peripheralUuid">> => PeripheralUuid,
		<<"serviceUuid">> => ServiceUuid,
        <<"serviceUuids">> => ServiceUuids
    }.

discover_characteristics(PeripheralUuid, ServiceUuid, CharacteristicUuids)
        when is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_list(CharacteristicUuids) ->
    #{
        <<"action">> => <<"discoverCharacteristics">>,
        <<"peripheralUuid">> => PeripheralUuid,
        <<"serviceUuid">> => ServiceUuid,
        <<"characteristicUuids">> => CharacteristicUuids
    }.

read(PeripheralUuid, ServiceUuid, CharacteristicUuid)
        when is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid) ->
    #{
        <<"action">> => <<"read">>,
        <<"peripheralUuid">> => PeripheralUuid,
        <<"serviceUuid">> => ServiceUuid,
        <<"characteristicUuid">> => CharacteristicUuid
    }.

write(PeripheralUuid, ServiceUuid, CharacteristicUuid, Data, WithoutResponse)
        when is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid)
		andalso is_binary(Data)
		andalso is_boolean(WithoutResponse) ->
    #{
        <<"action">> => <<"write">>,
        <<"peripheralUuid">> => PeripheralUuid,
        <<"serviceUuid">> => ServiceUuid,
        <<"characteristicUuid">> => CharacteristicUuid,
		<<"data">> => Data,
		<<"withoutResponse">> => WithoutResponse
    }.

broadcast(PeripheralUuid, ServiceUuid, CharacteristicUuid, Broadcast)
        when is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid)
		andalso is_boolean(Broadcast) ->
    #{
        <<"action">> => <<"broadcast">>,
        <<"peripheralUuid">> => PeripheralUuid,
        <<"serviceUuid">> => ServiceUuid,
        <<"characteristicUuid">> => CharacteristicUuid,
		<<"broadcast">> => Broadcast
    }.

notify(PeripheralUuid, ServiceUuid, CharacteristicUuid, Notify)
        when is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid)
		andalso is_boolean(Notify) ->
    #{
        <<"action">> => <<"notify">>,
        <<"peripheralUuid">> => PeripheralUuid,
        <<"serviceUuid">> => ServiceUuid,
        <<"characteristicUuid">> => CharacteristicUuid,
		<<"notify">> => Notify
    }.

discover_descriptors(PeripheralUuid, ServiceUuid, CharacteristicUuid)
        when is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid) ->
    #{
        <<"action">> => <<"discoverDescriptors">>,
        <<"peripheralUuid">> => PeripheralUuid,
        <<"serviceUuid">> => ServiceUuid,
        <<"characteristicUuid">> => CharacteristicUuid
    }.

read_value(PeripheralUuid, ServiceUuid, CharacteristicUuid, DescriptorUuid)
        when is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid)
		andalso is_binary(DescriptorUuid) ->
    #{
        <<"action">> => <<"readValue">>,
        <<"peripheralUuid">> => PeripheralUuid,
        <<"serviceUuid">> => ServiceUuid,
        <<"characteristicUuid">> => CharacteristicUuid,
		<<"descriptorUuid">> => DescriptorUuid
    }.

write_value(PeripheralUuid, ServiceUuid, CharacteristicUuid, DescriptorUuid, Data)
        when is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid)
		andalso is_binary(DescriptorUuid)
		andalso is_binary(Data) ->
    #{
        <<"action">> => <<"writeValue">>,
        <<"peripheralUuid">> => PeripheralUuid,
        <<"serviceUuid">> => ServiceUuid,
        <<"characteristicUuid">> => CharacteristicUuid,
		<<"descriptorUuid">> => DescriptorUuid,
		<<"data">> => Data
    }.

read_handle(PeripheralUuid, Handle)
        when is_binary(PeripheralUuid)
        andalso is_binary(Handle) ->
    #{
        <<"action">> => <<"readHandle">>,
        <<"peripheralUuid">> => PeripheralUuid,
        <<"handle">> => Handle
    }.

write_handle(PeripheralUuid, Handle, Data, WithoutResponse)
        when is_binary(PeripheralUuid)
        andalso is_binary(Handle)
		andalso is_binary(Data)
		andalso is_boolean(WithoutResponse) ->
    #{
        <<"action">> => <<"writeHandle">>,
        <<"peripheralUuid">> => PeripheralUuid,
        <<"handle">> => Handle,
		<<"data">> => Data,
		<<"withoutResponse">> => WithoutResponse
    }.

ping(Data)
		when is_binary(Data) ->
	#{
		<<"action">> => <<"ping">>,
		<<"data">> => Data
	}.

stop() ->
	#{
		<<"action">> => <<"stop">>
	}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
maybe_decode_command(Command = #{<<"action">> := _}) ->
	case Command of
		#{<<"action">> := <<"write">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'decode16!'(Data0, [{'case', 'mixed'}]),
			Command#{<<"data">> := Data1};
		#{<<"action">> := <<"writeValue">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'decode16!'(Data0, [{'case', 'mixed'}]),
			Command#{<<"data">> := Data1};
		#{<<"action">> := <<"writeHandle">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'decode16!'(Data0, [{'case', 'mixed'}]),
			Command#{<<"data">> := Data1};
		#{<<"action">> := <<"ping">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'decode16!'(Data0, [{'case', 'mixed'}]),
			Command#{<<"data">> := Data1};
		_ ->
			Command
	end.

%% @private
maybe_encode_command(Command = #{<<"action">> := _}) ->
	case Command of
		#{<<"action">> := <<"write">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'encode16'(Data0, [{'case', 'lower'}]),
			Command#{<<"data">> := Data1};
		#{<<"action">> := <<"writeValue">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'encode16'(Data0, [{'case', 'lower'}]),
			Command#{<<"data">> := Data1};
		#{<<"action">> := <<"writeHandle">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'encode16'(Data0, [{'case', 'lower'}]),
			Command#{<<"data">> := Data1};
		#{<<"action">> := <<"ping">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'encode16'(Data0, [{'case', 'lower'}]),
			Command#{<<"data">> := Data1};
		_ ->
			Command
	end.

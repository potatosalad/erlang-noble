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
-module(noble_event).

%% Public API
-export([decode/1]).
-export([encode/1]).
-export([schema/1]).
-export([validate/1]).
%% Event API

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
	maybe_decode_event('Elixir.Jason':'decode!'(Input)).

encode(Event = #{<<"type">> := _}) ->
	'Elixir.Jason':'encode!'(maybe_encode_event(Event)).

schema(#{<<"type">> := Type}) ->
	schema(Type);
schema(<< C, Rest/binary >>) ->
	Type = << (?UC(C)), Rest/binary >>,
	Definitions = ets:lookup_element(noble_schema, event, 2),
	maps:find(Type, Definitions);
schema(_) ->
	error.

validate(Event = #{<<"type">> := _}) ->
	{ok, EventSchema} = schema(<<"Event">>),
	case 'Elixir.JsonXema':'validate'(EventSchema, Event) of
		ok ->
			ok;
		EventSchemaError ->
			%% Try to validate against a more specific version of the event schema.
			case schema(Event) of
				{ok, TypeSchema} ->
					case 'Elixir.JsonXema':'validate'(TypeSchema, Event) of
						ok ->
							EventSchemaError;
						TypeSchemaError ->
							TypeSchemaError
					end;
				error ->
					EventSchemaError
			end
	end.

%%%===================================================================
%%% Event API
%%%===================================================================

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
maybe_decode_event(Event = #{<<"type">> := _}) ->
	case Event of
		#{<<"type">> := <<"read">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'decode16!'(Data0, [{'case', 'mixed'}]),
			Event#{<<"data">> := Data1};
		#{<<"type">> := <<"valueRead">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'decode16!'(Data0, [{'case', 'mixed'}]),
			Event#{<<"data">> := Data1};
		#{<<"type">> := <<"handleRead">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'decode16!'(Data0, [{'case', 'mixed'}]),
			Event#{<<"data">> := Data1};
		#{<<"type">> := <<"handleNotify">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'decode16!'(Data0, [{'case', 'mixed'}]),
			Event#{<<"data">> := Data1};
		#{<<"type">> := <<"discover">>, <<"advertisement">> := Advertisement0} when is_map(Advertisement0) ->
			Advertisement1 = maybe_decode_advertisement(Advertisement0),
			Event#{<<"advertisement">> := Advertisement1};
		#{<<"type">> := <<"pong">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'decode16!'(Data0, [{'case', 'mixed'}]),
			Event#{<<"data">> := Data1};
		_ ->
			Event
	end.

%% @private
maybe_decode_advertisement(Advertisement0) when is_map(Advertisement0) ->
	Advertisement1 =
		case Advertisement0 of
			#{<<"manufacturerData">> := ManufacturerData0} when is_binary(ManufacturerData0) ->
				ManufacturerData1 = 'Elixir.Base':'decode16!'(ManufacturerData0, [{'case', 'mixed'}]),
				Advertisement0#{<<"manufacturerData">> := ManufacturerData1};
			_ ->
				Advertisement0
		end,
	Advertisement2 =
		case Advertisement1 of
			#{<<"serviceData">> := ServiceData0} when is_list(ServiceData0) ->
				ServiceData1 = [maybe_decode_service_data(ServiceData) || ServiceData <- ServiceData0],
				Advertisement1#{<<"serviceData">> := ServiceData1};
			_ ->
				Advertisement1
		end,
	Advertisement2.

%% @private
maybe_decode_service_data(ServiceData) when is_map(ServiceData) ->
	case ServiceData of
		#{<<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'decode16!'(Data0, [{'case', 'mixed'}]),
			ServiceData#{<<"data">> := Data1};
		_ ->
			ServiceData
	end.

%% @private
maybe_encode_event(Event = #{<<"type">> := _}) ->
	case Event of
		#{<<"type">> := <<"read">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'encode16!'(Data0, [{'case', 'lower'}]),
			Event#{<<"data">> := Data1};
		#{<<"type">> := <<"valueRead">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'encode16!'(Data0, [{'case', 'lower'}]),
			Event#{<<"data">> := Data1};
		#{<<"type">> := <<"handleRead">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'encode16!'(Data0, [{'case', 'lower'}]),
			Event#{<<"data">> := Data1};
		#{<<"type">> := <<"handleNotify">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'encode16!'(Data0, [{'case', 'lower'}]),
			Event#{<<"data">> := Data1};
		#{<<"type">> := <<"discover">>, <<"advertisement">> := Advertisement0} when is_map(Advertisement0) ->
			Advertisement1 = maybe_encode_advertisement(Advertisement0),
			Event#{<<"advertisement">> := Advertisement1};
		#{<<"type">> := <<"pong">>, <<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'encode16!'(Data0, [{'case', 'lower'}]),
			Event#{<<"data">> := Data1};
		_ ->
			Event
	end.

%% @private
maybe_encode_advertisement(Advertisement0) when is_map(Advertisement0) ->
	Advertisement1 =
		case Advertisement0 of
			#{<<"manufacturerData">> := ManufacturerData0} when is_binary(ManufacturerData0) ->
				ManufacturerData1 = 'Elixir.Base':'encode16!'(ManufacturerData0, [{'case', 'lower'}]),
				Advertisement0#{<<"manufacturerData">> := ManufacturerData1};
			_ ->
				Advertisement0
		end,
	Advertisement2 =
		case Advertisement1 of
			#{<<"serviceData">> := ServiceData0} when is_list(ServiceData0) ->
				ServiceData1 = [maybe_encode_service_data(ServiceData) || ServiceData <- ServiceData0],
				Advertisement1#{<<"serviceData">> := ServiceData1};
			_ ->
				Advertisement1
		end,
	Advertisement2.

%% @private
maybe_encode_service_data(ServiceData) when is_map(ServiceData) ->
	case ServiceData of
		#{<<"data">> := Data0} when is_binary(Data0) ->
			Data1 = 'Elixir.Base':'encode16!'(Data0, [{'case', 'lower'}]),
			ServiceData#{<<"data">> := Data1};
		_ ->
			ServiceData
	end.

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
-module(noble).

%% Public API
-export([open/0]).
-export([open/1]).
-export([close/1]).
-export([controlling_process/2]).
-export([start_scanning/1]).
-export([start_scanning/2]).
-export([start_scanning/3]).
-export([stop_scanning/1]).
-export([connect/2]).
-export([disconnect/2]).
-export([update_rssi/2]).
-export([discover_services/3]).
-export([discover_included_services/4]).
-export([discover_characteristics/4]).
-export([read/4]).
-export([write/6]).
-export([broadcast/5]).
-export([notify/5]).
-export([discover_descriptors/4]).
-export([read_value/5]).
-export([write_value/6]).
-export([read_handle/3]).
-export([write_handle/5]).
-export([ping/2]).
-export([stop/1]).
%% Internal API
-export([priv_dir/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

open() ->
	{ok, Port} = noble_port:open(),
	Port.

open(Owner) when is_pid(Owner) ->
	{ok, Port} = noble_port:open(Owner),
	Port.

close(Port) when is_pid(Port) ->
	noble_port:close(Port).

controlling_process(Port, NewOwner) ->
	noble_port:controlling_process(Port, NewOwner).

start_scanning(Port)
		when is_pid(Port) ->
	Command = noble_command:start_scanning(),
	true = noble_port:port_command(Port, Command),
	ok.

start_scanning(Port, ServiceUuids)
		when is_pid(Port)
		andalso is_list(ServiceUuids) ->
	Command = noble_command:start_scanning(ServiceUuids),
	true = noble_port:port_command(Port, Command),
	ok;
start_scanning(Port, AllowDuplicates)
		when is_pid(Port)
		andalso is_boolean(AllowDuplicates) ->
	Command = noble_command:start_scanning(AllowDuplicates),
	true = noble_port:port_command(Port, Command),
	ok.

start_scanning(Port, ServiceUuids, AllowDuplicates)
		when is_pid(Port)
		andalso is_list(ServiceUuids)
		andalso is_boolean(AllowDuplicates) ->
	Command = noble_command:start_scanning(ServiceUuids, AllowDuplicates),
	true = noble_port:port_command(Port, Command),
	ok.

stop_scanning(Port)
		when is_pid(Port) ->
	Command = noble_command:stop_scanning(),
	true = noble_port:port_command(Port, Command),
	ok.

connect(Port, PeripheralUuid)
		when is_pid(Port)
		andalso is_binary(PeripheralUuid) ->
	Command = noble_command:connect(PeripheralUuid),
	true = noble_port:port_command(Port, Command),
	ok.

disconnect(Port, PeripheralUuid)
		when is_pid(Port)
		andalso is_binary(PeripheralUuid) ->
	Command = noble_command:disconnect(PeripheralUuid),
	true = noble_port:port_command(Port, Command),
	ok.

update_rssi(Port, PeripheralUuid)
		when is_pid(Port)
		andalso is_binary(PeripheralUuid) ->
    Command = noble_command:update_rssi(PeripheralUuid),
	true = noble_port:port_command(Port, Command),
	ok.

discover_services(Port, PeripheralUuid, ServiceUuids)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_list(ServiceUuids) ->
    Command = noble_command:discover_services(PeripheralUuid, ServiceUuids),
	true = noble_port:port_command(Port, Command),
	ok.

discover_included_services(Port, PeripheralUuid, ServiceUuid, ServiceUuids)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_list(ServiceUuids) ->
    Command = noble_command:discover_included_services(PeripheralUuid, ServiceUuid, ServiceUuids),
	true = noble_port:port_command(Port, Command),
	ok.

discover_characteristics(Port, PeripheralUuid, ServiceUuid, CharacteristicUuids)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_list(CharacteristicUuids) ->
    Command = noble_command:discover_characteristics(PeripheralUuid, ServiceUuid, CharacteristicUuids),
	true = noble_port:port_command(Port, Command),
	ok.

read(Port, PeripheralUuid, ServiceUuid, CharacteristicUuid)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid) ->
    Command = noble_command:read(PeripheralUuid, ServiceUuid, CharacteristicUuid),
	true = noble_port:port_command(Port, Command),
	ok.

write(Port, PeripheralUuid, ServiceUuid, CharacteristicUuid, Data, WithoutResponse)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid)
		andalso is_binary(Data)
		andalso is_boolean(WithoutResponse) ->
    Command = noble_command:write(PeripheralUuid, ServiceUuid, CharacteristicUuid, Data, WithoutResponse),
	true = noble_port:port_command(Port, Command),
	ok.

broadcast(Port, PeripheralUuid, ServiceUuid, CharacteristicUuid, Broadcast)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid)
		andalso is_boolean(Broadcast) ->
    Command = noble_command:broadcast(PeripheralUuid, ServiceUuid, CharacteristicUuid, Broadcast),
	true = noble_port:port_command(Port, Command),
	ok.

notify(Port, PeripheralUuid, ServiceUuid, CharacteristicUuid, Notify)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid)
		andalso is_boolean(Notify) ->
    Command = noble_command:notify(PeripheralUuid, ServiceUuid, CharacteristicUuid, Notify),
	true = noble_port:port_command(Port, Command),
	ok.

discover_descriptors(Port, PeripheralUuid, ServiceUuid, CharacteristicUuid)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid) ->
    Command = noble_command:discover_descriptors(PeripheralUuid, ServiceUuid, CharacteristicUuid),
	true = noble_port:port_command(Port, Command),
	ok.

read_value(Port, PeripheralUuid, ServiceUuid, CharacteristicUuid, DescriptorUuid)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid)
		andalso is_binary(DescriptorUuid) ->
    Command = noble_command:read_value(PeripheralUuid, ServiceUuid, CharacteristicUuid, DescriptorUuid),
	true = noble_port:port_command(Port, Command),
	ok.

write_value(Port, PeripheralUuid, ServiceUuid, CharacteristicUuid, DescriptorUuid, Data)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_binary(ServiceUuid)
        andalso is_binary(CharacteristicUuid)
		andalso is_binary(DescriptorUuid)
		andalso is_binary(Data) ->
    Command = noble_command:write_value(PeripheralUuid, ServiceUuid, CharacteristicUuid, DescriptorUuid, Data),
	true = noble_port:port_command(Port, Command),
	ok.

read_handle(Port, PeripheralUuid, Handle)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_binary(Handle) ->
    Command = noble_command:read_handle(PeripheralUuid, Handle),
	true = noble_port:port_command(Port, Command),
	ok.

write_handle(Port, PeripheralUuid, Handle, Data, WithoutResponse)
        when is_pid(Port)
		andalso is_binary(PeripheralUuid)
        andalso is_binary(Handle)
		andalso is_binary(Data)
		andalso is_boolean(WithoutResponse) ->
    Command = noble_command:write_handle(PeripheralUuid, Handle, Data, WithoutResponse),
	true = noble_port:port_command(Port, Command),
	ok.

ping(Port, Data)
		when is_pid(Port)
		andalso is_binary(Data) ->
	Command = noble_command:ping(Data),
	true = noble_port:port_command(Port, Command),
	ok.

stop(Port)
		when is_pid(Port) ->
	Command = noble_command:stop(),
	true = noble_port:port_command(Port, Command),
	ok.

%%%===================================================================
%%% Internal API
%%%===================================================================

-spec priv_dir() -> file:filename_all().
priv_dir() ->
	case code:priv_dir(?MODULE) of
		{error, bad_name} ->
			case code:which(?MODULE) of
				Filename when is_list(Filename) ->
					filename:join([filename:dirname(Filename), "../priv"]);
				_ ->
					"../priv"
			end;
		Dir ->
			Dir
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

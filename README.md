# Noble

A Node.js BLE (Bluetooth Low Energy) port for Erlang and Elixir.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `noble` to your list of dependencies in `mix.exs`:

```elixir
def deps() do
  [
    {:noble, "~> 0.0.4"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/noble](https://hexdocs.pm/noble).

## Usage

```elixir
iex> port = :noble.open()
#PID<0.208.0>
iex> flush()
{:noble_port_data, #PID<0.208.0>,
 %{"state" => "unknown", "type" => "stateChange"}}
{:noble_port_data, #PID<0.208.0>,
 %{"state" => "poweredOn", "type" => "stateChange"}}
iex> :noble.start_scanning(port, ["3c0224177edd82f015c558dba740760f"])
:ok
iex> flush()
{:noble_port_data, #PID<0.208.0>, %{"type" => "scanStart"}}
{:noble_port_data, #PID<0.208.0>,
 %{
   "address" => "04-6a-fd-20-d9-ee",
   "addressType" => "public",
   "advertisement" => %{
     "localName" => "MY_BLE_DEVICE",
     "manufacturerData" => nil,
     "serviceData" => nil,
     "serviceUuids" => ["3c0224177edd82f015c558dba740760f"],
     "txPowerLevel" => nil
   },
   "connectable" => true,
   "peripheralUuid" => "75f2b516030ffbfd694125abf5f73337",
   "rssi" => -78,
   "type" => "discover"
 }}
iex> :noble.stop_scanning(port)
:ok
iex> flush()
{:noble_port_data, #PID<0.208.0>, %{"type" => "scanStop"}}
iex> :noble.connect(port, "75f2b516030ffbfd694125abf5f73337")
:ok
iex> flush()
{:noble_port_data, #PID<0.208.0>,
 %{"peripheralUuid" => "75f2b516030ffbfd694125abf5f73337", "type" => "connect"}}
iex> :noble.disconnect(port, "75f2b516030ffbfd694125abf5f73337")
:ok
iex> flush()
{:noble_port_data, #PID<0.208.0>,
 %{"peripheralUuid" => "75f2b516030ffbfd694125abf5f73337", "type" => "disconnect"}}
iex> :noble.stop(port)
:ok
iex> flush()
{:noble_port_data, #PID<0.208.0>, %{"type" => "stop"}}
{:noble_port_closed, #PID<0.208.0>}
```

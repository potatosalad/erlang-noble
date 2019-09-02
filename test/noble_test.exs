defmodule NobleTest do
  use ExUnit.Case

  test ":noble_port smoke test" do
    {:ok, _} = :application.ensure_all_started(:noble)
    {:ok, port} = :noble_port.open()
    assert_receive {:noble_port_data, ^port, %{"type" => "stateChange", "state" => _}}, 5_000
    true = :noble_port.port_command(port, :noble_command.ping("test"))
    assert_receive {:noble_port_data, ^port, %{"type" => "pong", "data" => "test"}}, 5_000
    true = :noble_port.port_command(port, :noble_command.stop())
    assert_receive {:noble_port_data, ^port, %{"type" => "stop"}}, 5_000
    :ok = :noble_port.close(port)
    assert_receive {:noble_port_closed, ^port}, 5_000
  end
end

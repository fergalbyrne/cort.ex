defmodule ServerTest do
  use ExUnit.Case
  alias Cortex.Network

  @vsn "0.0.1"

  setup_all do
	net1 = %Network{ref: make_ref, name: "net1"}
	net2 = %Network{ref: make_ref, name: "net2"}
	{ :ok, server_pid } = Cortex.Server.start_link([])
	{ :ok, server_pid: server_pid, net1: net1, net2: net2}
  end

  test "server does not contain net2", meta do
	net2 = meta[:net2]
	server_pid = meta[:server_pid]
    assert :gen_server.call(server_pid, {:pid, net2.ref}) == :undef
  end

  test "new network can be found", meta do
	net1 = meta[:net1]
	server_pid = meta[:server_pid]
    assert :gen_server.call(server_pid, {:pid, net1.ref}) == :undef
    :gen_server.cast(server_pid, {:add, net1}) 
    assert :gen_server.call(server_pid, {:pid, net1.ref}) == net1.pid
  end
  

end

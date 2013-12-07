defmodule ServerTest do
  use ExUnit.Case
  alias Nupic.Network.NetworkInfo

  @vsn "0.0.1"

  setup_all do
	net1 = NetworkInfo.new(ref: make_ref, name: "net1")
	{ :ok, server_pid } = Nupic.Server.start_link([])
	{ :ok, server_pid: server_pid, net1: net1}
  end

  test "new server does not contain net1", meta do
	net1 = meta[:net1]
	server_pid = meta[:server_pid]
    assert :gen_server.call(server_pid, {:pid, net1.ref}) == :undef
  end

  test "new network can be found", meta do
	net1 = meta[:net1]
	server_pid = meta[:server_pid]
    assert :gen_server.call(server_pid, {:pid, net1.ref}) == :undef
    :gen_server.cast(server_pid, {:add, net1}) 
  end
  
  teardown meta do
	:gen_server.cast(meta[:server_pid], {:delete, meta[:net1].ref}) 
  end

end

defmodule PatchTest do
  use ExUnit.Case

  @vsn "0.0.1"

  setup_all do
	p = PatchInfo.new
	n1 = NeuronInfo.new(ref: make_ref)
	{ :ok, server_pid } = CLA.Patch.start_link(p)
	{ :ok, server_pid: server_pid, n1: n1, p: p}
  end

  test "new patch does not contain n1", meta do
	n1 = meta[:n1]
	server_pid = meta[:server_pid]
    assert :gen_server.call(server_pid, {:pid, n1.ref}) == :undef
  end

  test "new neuron can be found", meta do
	n1 = meta[:n1]
	server_pid = meta[:server_pid]
    assert :gen_server.call(server_pid, {:pid, n1.ref}) == :undef
    :gen_server.cast(server_pid, {:add, n1}) 
  end

  test "two neurons should be connected", meta do
	n1 = meta[:n1]
	server_pid = meta[:server_pid]
    assert :gen_server.call(server_pid, {:pid, n1.ref}) == :undef
	{check_neuron, check_neuron2, check_pid, check_pid2, n2} = connect_two_neurons meta
	assert check_neuron.feedforward == [{n2.ref, 0.2, check_pid2}]
	assert check_neuron2.axons == [{n1.ref, 0.2, check_pid}]
  end

  test "activation raised by feedforward", meta do
	n1 = meta[:n1]
	server_pid = meta[:server_pid]
    assert :gen_server.call(server_pid, {:pid, n1.ref}) == :undef
	{check_neuron, check_neuron2, check_pid, check_pid2, n2} = connect_two_neurons meta
	:gen_server.cast(check_neuron2.pid,{:force_spike, 1})
	affected_neuron = get_neuron n1.ref, meta
	assert affected_neuron.activation == check_neuron.activation + 1
  end

  test "prediction raised by distal", meta do
	n1 = meta[:n1]
	server_pid = meta[:server_pid]
    assert :gen_server.call(server_pid, {:pid, n1.ref}) == :undef
	{check_neuron, check_neuron2, check_pid, check_pid2, n2} = connect_two_neurons meta
	:gen_server.cast(check_pid, {:distal, n2.pid, n2.ref}) 
	:gen_server.cast(check_neuron2.pid,{:force_spike, 1})
	affected_neuron = get_neuron n1.ref, meta
	assert affected_neuron.predictivity == check_neuron.predictivity + 1
  end

  test "loads of neurons", meta do
	{ :ok, big_server_pid } = CLA.Patch.start_link(PatchInfo.new)
	n1 = meta[:n1]
	#server_pid = meta[:server_pid]
	n = 2048 * 1
	n = 32
	mult = 1024
	kmult = 32
	neurons = 1..(n * mult)
		|>
	Enum.map(fn(i) -> 
		neuron = make_ref
		:gen_server.cast(big_server_pid, {:add, n1.ref(neuron)}) 
		neuron
		end)
	#neurons = :gen_server.call(big_server_pid, {:neurons})
	assert length(neurons) == n * mult
	last = hd(:lists.reverse neurons)
	found = :gen_server.call(big_server_pid, {:pid, last})
	last_neuron = :gen_server.call(found, {:dump})
	assert last_neuron.ref == last
	#1..n |> Enum.map(fn(i) -> :gen_server.cast(big_server_pid, {:add, n1.ref(make_ref)}) end)
  end
  
  teardown meta do
	:gen_server.cast(meta[:server_pid], {:delete, meta[:n1].ref}) 
  end

  defp get_neuron ref, meta do
	server_pid = meta[:server_pid]
	check_pid = :gen_server.call(server_pid, {:pid, ref})
	:gen_server.call(check_pid, {:dump})
  end

  defp connect_two_neurons meta do
	n1 = meta[:n1]
	server_pid = meta[:server_pid]
    :gen_server.cast(server_pid, {:add, n1})
	n2 = n1.ref(make_ref)
	:gen_server.cast(server_pid, {:add, n2}) 
	:gen_server.cast(server_pid, {:feedforward, n1.ref, n2.ref}) 
	check_pid = :gen_server.call(server_pid, {:pid, n1.ref})
	check_pid2 = :gen_server.call(server_pid, {:pid, n2.ref})
	check_neuron = :gen_server.call(check_pid, {:dump})
	check_neuron2 = :gen_server.call(check_pid2, {:dump})
	{check_neuron, check_neuron2, check_pid, check_pid2, n2}
  end

end

defmodule CLA.Patch do
  use GenServer.Behaviour
  alias CLA.Neuron.NeuronInfo


  @moduledoc """
Manages a patch of Neurons in NuPIC.
"""

  @doc """

n1ref = make_ref                          
n1 = CLA.Neuron.NeuronInfo.new(ref: n1ref)
{ok, patch} = CLA.Patch.start_link([])    
:gen_server.cast(patch, {:add, n1})   
:gen_server.call(patch, {:pid, n1ref})    
"""

  def start_link(neurons) do
    :gen_server.start_link(__MODULE__, neurons, [])
  end

  def init(neurons) do
	self <- {:load_neurons} # once loaded, activate my neurons
    { :ok, neurons }
  end

  def handle_call({:pid, neuron_ref}, _from, neurons) do
	case neuron_by_ref(neuron_ref, neurons) do
	  false -> {:reply, :undef, neurons}
	  neuron -> {:reply, neuron.pid, neurons}
	end
  end

  def handle_call({:neurons}, _from, neurons) do
	{:reply, neurons, neurons}
  end

  def handle_cast({ :add, new }, neurons) do
    { :noreply, add(new, neurons) }
  end

  def handle_cast({ :delete, neuron_ref }, neurons) do
	neuron = neuron_by_ref(neuron_ref, neurons)
    { :noreply,  remove(neuron, neurons)}
  end

  def handle_cast({ :feedforward, neuron1, neuron2 }, neurons) do
	#IO.puts "Connecting #{neuron1} to #{neuron2}"
	n1 = neuron_by_ref(neuron1, neurons)
	n2 = neuron_by_ref(neuron2, neurons)
	:gen_server.cast(n1.pid, {:feedforward, n2.pid, neuron2})
    { :noreply,  neurons}
  end

  defp neuron_by_ref(neuron_ref, neurons) do
	Enum.find neurons, false, fn(NeuronInfo[ref: r]) -> r == neuron_ref end
  end

  defp add(item, neurons) do
	{ :ok, pid } = CLA.Neuron.start_link(item)
	item = item.pid(pid)
	[item|neurons]
  end 

  defp remove(item, neurons) do
	#TODO: kill neuron process
	List.delete(neurons,item)
  end

end
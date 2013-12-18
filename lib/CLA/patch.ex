defmodule CLA.Patch do
  use GenServer.Behaviour
  @vsn "0.0.2"
  @max 256

  @moduledoc """
Manages a patch of Neurons in NuPIC.
"""

  @doc """

n1ref = make_ref                          
n1 = NeuronInfo.new(ref: n1ref)
p = PatchInfo.new
{ok, patch} = CLA.Patch.start_link(p)    
:gen_server.cast(patch, {:add, n1})   
:gen_server.call(patch, {:pid, n1ref})    
"""

  def start_link(patch) do
    :gen_server.start_link(__MODULE__, patch, [])
  end

  def init(patch) do
	self <- {:load_neurons} # once loaded, activate my neurons
    { :ok, patch.pid(self) }
  end

  def handle_call({:pid, neuron_ref}, _from, patch) do
	case neuron_by_ref(neuron_ref, patch) do
	  false -> {:reply, :undef, patch}
	  neuron -> {:reply, neuron.pid, patch}
	end
  end

  def handle_call({:find, neuron_ref}, _from, patch) do
	{:reply, neuron_by_ref(neuron_ref, patch), patch}
  end

  def handle_call({:neurons}, _from, patch) do
	{:reply, patch, patch}
  end

  def handle_cast({ :add, new }, patch) do
    { :noreply, add(new, patch) }
  end

  def handle_cast({ :delete, neuron_ref }, patch) do
	neuron = neuron_by_ref(neuron_ref, patch)
    { :noreply,  remove(neuron, patch)}
  end

  def handle_cast({ :feedforward, neuron1, neuron2 }, patch) do
	#IO.puts "Connecting #{neuron1} to #{neuron2}"
	n1 = neuron_by_ref(neuron1, patch)
	n2 = neuron_by_ref(neuron2, patch)
	:gen_server.cast(n1.pid, {:feedforward, n2.pid, neuron2})
    { :noreply,  patch}
  end

  defp neuron_by_ref(neuron_ref, patch = PatchInfo[min_ref: nil, max_ref: nil]) do
	Enum.find patch.neurons, false, fn(NeuronInfo[ref: r]) -> r == neuron_ref end
  end
  defp neuron_by_ref(neuron_ref, patch = PatchInfo[min_ref: min, max_ref: max]) 
		when (neuron_ref >= min and neuron_ref <= max) do
	Enum.find patch.neurons, false, fn(NeuronInfo[ref: r]) -> r == neuron_ref end
  end
  defp neuron_by_ref(neuron_ref, patch = PatchInfo[min_ref: min, max_ref: max, patches: {nil, _}]) 
		when (neuron_ref < min) do
	false
  end
  defp neuron_by_ref(neuron_ref, patch = PatchInfo[min_ref: min, max_ref: max, patches: {nil, _}]) 
		when (neuron_ref > max) do
	false
  end
  defp neuron_by_ref(neuron_ref, patch = PatchInfo[min_ref: min, max_ref: max, patches: {pid, _}]) 
		when (neuron_ref < min) do
	:gen_server.call(pid, {:find, neuron_ref})
  end
  defp neuron_by_ref(neuron_ref, patch = PatchInfo[min_ref: min, max_ref: max, patches: {_, pid}]) 
		when (neuron_ref > max) do
	:gen_server.call(pid, {:find, neuron_ref})
  end


  defp add(item, patch = PatchInfo[max: max]) do
	case length patch.neurons do
		n when n < max -> (
			#IO.puts "adding neuron #{n} to patch #{inspect self}"
			{ :ok, pid } = CLA.Neuron.start_link(item)
			item = item.pid(pid)
			new_max = if patch.max_ref == nil or patch.max_ref < item.ref, do: item.ref, else: patch.max_ref
			new_min = if patch.min_ref == nil or patch.min_ref > item.ref, do: item.ref, else: patch.min_ref
			patch.neurons([item|patch.neurons]).max_ref(new_max).min_ref(new_min)
		)
		_ -> (
			#IO.puts "adding neuron to subpatch of #{inspect self}"
			add_to_subpatch(item, patch)
		)
	end	
  end 

  defp add_to_subpatch(item, patch = PatchInfo[patches: {nil,_}]) do
	#{ :ok, subpatch } = start_link(PatchInfo.new)
	#IO.puts "new subpatch starting"
	{ :ok, left } = :gen_server.start_link(__MODULE__, PatchInfo.new, [])
	{ :ok, right } = :gen_server.start_link(__MODULE__, PatchInfo.new, [])
	:gen_server.cast(self, {:add, item})
	patch.patches({left,right})
  end
  defp add_to_subpatch(item = NeuronInfo[ref: ref], patch = PatchInfo[min_ref: min, patches: {subpatch,_}]) 
		when ref < min do
	#IO.puts "sending new neuron to subpatch #{inspect subpatch} to add"
	:gen_server.cast(subpatch, {:add, item})
	patch
  end
  defp add_to_subpatch(item = NeuronInfo[ref: ref], patch = PatchInfo[max_ref: max, patches: {_,subpatch}]) 
		when ref > max do
	#IO.puts "sending new neuron to subpatch #{inspect subpatch} to add"
	:gen_server.cast(subpatch, {:add, item})
	patch
  end

  defp remove(item, patch) do
	#TODO: kill neuron process
	patch.neurons(List.delete(patch.neurons,item))
  end

end
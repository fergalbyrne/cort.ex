defmodule CLA.Neuron do
	use GenServer.Behaviour
	@vsn "0.0.2"
  @moduledoc """
Models a CLA neuron

* Examples:

Create a new neuron

	iex> n = Neuron.create() 
	NeuronInfo[ref: nil, pid: nil, activation: 0, predictivity: 0, feedforward: [], dendrites: []]
	iex> a = n.activation
	0

"""

	def start_link(info) do
	  :gen_server.start_link(__MODULE__, info, [])
	end

	def init(info) do
		#self <- {:loaded} # once loaded, say hi to the other neurons
		:gen_server.cast(self,{:loaded})
		{ :ok, info }
	end

	def handle_call({:dump}, _from, info) do
		{:reply, info, info}
	end
	
	def handle_cast({:loaded}, info) do
		info = info.pid(self)
		#IO.puts "Neuron loaded: #{info.ref}"
		{ :noreply, info}
	end

	def handle_cast({:feedforward, from_id, from_ref}, info) do
		unless Enum.find(info.feedforward, fn({from_ref,_,_}) -> true end) do
			info = info.feedforward([{from_ref, 0.2, from_id}|info.feedforward])
			:gen_server.cast(from_id, {:axon, info.pid, info.ref})
			#IO.puts "Neuron loaded: #{info.ref}"
		end
		{ :noreply, info}
	end

	def handle_cast({:distal, from_id, from_ref}, info) do
		unless Enum.find(info.dendrites, fn({from_ref,_,_}) -> true end) do
			info = info.dendrites([{from_ref, 0.2, from_id}|info.dendrites])
			:gen_server.cast(from_id, {:axon, info.pid, info.ref})
			#IO.puts "Neuron loaded: #{info.ref}"
		end
		{ :noreply, info}
	end

	def handle_cast({:axon, to_id, to_ref}, info) do
		unless Enum.find(info.axons, fn({to_ref,_,_}) -> true end) do
			info = info.axons([{to_ref, 0.2, to_id}|info.axons])
			#IO.puts "Neuron loaded: #{info.ref}"
		end
		{ :noreply, info}
	end

	def handle_cast({:signal, from_ref, _value}, info) do
		entry = Enum.find(info.feedforward, fn(entry) -> 
			case entry do
				{from_ref,_,_} -> entry 
				_ -> false
			end
		end)
		if entry do
			info = info.activation(info.activation + 1)
			{_,_,from_id} = entry
			#:gen_server.cast(from_id, {:fed_forward, info.ref})
			#IO.puts "Neuron loaded: #{info.ref}"
		end
		# check distal dendrites
		entry = Enum.find(info.dendrites, fn(entry) -> 
			case entry do
				{from_ref,_,_} -> entry 
				_ -> false
			end
		end)
		if entry do
			info = info.predictivity(info.predictivity + 1)
			{_,_,from_id} = entry
			#:gen_server.cast(from_id, {:fed_forward, info.ref})
			#IO.puts "Neuron loaded: #{info.ref}"
		end
		{ :noreply, info}
	end

	def handle_cast({:force_spike, value}, info) do
		info.axons
			|>
		Enum.map(fn(tup) -> 
			signal(tup, value, info)
		end)
		{ :noreply, info}
	end

	defp signal({to_ref, perm, to_pid}, value, info) when perm >= 0.1 do
		:gen_server.cast(to_pid, {:signal, info.ref, value})
	end
	defp signal(_) do
		nil
	end
	
  @doc """
Creates a new CLA neuron.

* Examples:

	iex> Neuron.create() 
	NeuronInfo[ref: nil, pid: nil, activation: 0, predictivity: 0, feedforward: [], dendrites: [], axons: []]

"""
	def create(), do: NeuronInfo.new
	def create(neuron), do: neuron

  @doc """
connect(n1, n2): Connects neuron n2 to neuron n1's distal dendrites.


* Examples:

	iex> n1 = Neuron.create() 
	NeuronInfo[ref: nil, pid: nil, activation: 0, predictivity: 0, feedforward: [], dendrites: [], axons: []]
	iex> n1 = n1.ref("n1")
	NeuronInfo[ref: "n1", pid: nil, activation: 0, predictivity: 0, feedforward: [], dendrites: [], axons: []]
	iex> n2 = Neuron.create()
	NeuronInfo[ref: nil, pid: nil, activation: 0, predictivity: 0, feedforward: [], dendrites: [], axons: []]
	iex> n2 = n2.ref("n2")
	NeuronInfo[ref: "n2", pid: nil, activation: 0, predictivity: 0, feedforward: [], dendrites: [], axons: []]
	iex> Neuron.connect(n1, n2)
	:ok

    """
	def connect(n1, n2info) do
		#newn1 = do_add_synapse(n1, n2info)
		#newn2 = do_add_axon(n2info, n1)
		:ok
	end
	
	
end
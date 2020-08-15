defmodule Cortex.Server do
	@vsn "0.0.1"
  use GenServer
  require Logger


  @moduledoc """
Manages the Networks in NuPIC. NuPIC 
"""

  @doc """
"""

  def start_link(networks) do
    :gen_server.start_link({ :local, :nupic_server }, __MODULE__, networks, [])
  end

  def init(networks) do
    { :ok, networks }
  end

  def handle_call({:pid, network_ref}, _from, networks) do
	Logger.debug "server.ex handle_call {#{:pid}, #{inspect network_ref}} networks: #{inspect networks}"
	case network_by_ref(network_ref, networks) do
	  false -> {:reply, :undef, networks}
	  network -> {:reply, network.pid, networks}
	end
  end

  def handle_cast({ :add, new }, networks) do
    { :noreply, add(new, networks) }
  end

  def handle_cast({ :delete, network_ref }, networks) do
	network = network_by_ref(network_ref, networks)
    { :noreply,  remove(network, networks)}
  end

  defp network_by_ref(_network_ref, []) do false end
  defp network_by_ref(network_ref, networks) do
	#IO.puts "network_by_ref"
	Enum.find(networks, false, fn(network) -> network.ref == network_ref end)
  end

  defp add(item, networks) do
	[item|networks]
  end 

  defp remove(item, networks) do
	List.delete(networks,item)
  end

end
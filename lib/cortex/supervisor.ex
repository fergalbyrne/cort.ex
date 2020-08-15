defmodule Cortex.Supervisor do
	@vsn "0.0.1"
	
  use Supervisor

  # A convenience to start the supervisor
  def start_link(networks) do
    :supervisor.start_link(__MODULE__, networks)
  end

  # The callback invoked when the supervisor starts
  def init(networks) do
    children = [ worker(Cortex.Server, [networks]) ]
    supervise children, strategy: :one_for_one
  end
end
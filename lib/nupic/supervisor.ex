defmodule Nupic.Supervisor do
  use Supervisor.Behaviour

  # A convenience to start the supervisor
  def start_link(networks) do
    :supervisor.start_link(__MODULE__, networks)
  end

  # The callback invoked when the supervisor starts
  def init(networks) do
    children = [ worker(Nupic.Server, [networks]) ]
    supervise children, strategy: :one_for_one
  end
end
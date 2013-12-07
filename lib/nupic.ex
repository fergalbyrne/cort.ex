defmodule Nupic do
	use Application.Behaviour
	
	def start(_type, networks) do
		Nupic.Supervisor.start_link(networks)
	end		
	
end

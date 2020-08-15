defmodule Cortex do
	@vsn "0.0.1"
	
	use Application
	
	def start(_type, networks) do
		Cortex.Supervisor.start_link(networks)
	end		
	
end

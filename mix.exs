defmodule Nupic.Mixfile do
  use Mix.Project

  def project do
    [ app: :cortex,
      version: "0.0.2",
      elixir: ">= 1.0.0",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    []
  end

  # Returns the list of dependencies in the format:
  # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
  #
  # To specify particular versions, regardless of the tag, do:
  # { :barbat, "~> 0.1", github: "elixir-lang/barbat.git" }
  defp deps do
    #[{:amrita,"0.2.4",[github: "josephwilk/amrita"]}]
    []
  end
end

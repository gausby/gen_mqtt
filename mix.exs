defmodule GenMQTT.Mixfile do
  use Mix.Project

  def project do
    [app: :gen_mqtt,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [{:vmq_commons, github: "erlio/vmq_commons", manager: :rebar3}]
  end
end

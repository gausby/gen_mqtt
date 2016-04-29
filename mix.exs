defmodule GenMQTT.Mixfile do
  use Mix.Project

  def project do
    [app: :gen_mqtt,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps,
     docs: [extras: ["README.md"]]]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [{:vmq_commons, github: "erlio/vmq_commons", manager: :rebar3},
     {:ex_doc, "~> 0.11", only: :dev},
     {:earmark, "~> 0.1", only: :dev}]
  end
end

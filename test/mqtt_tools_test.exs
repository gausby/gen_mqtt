defmodule MqttToolsTest do
  use ExUnit.Case
  doctest MqttTools

  defmodule TestModule do
    use MqttTools.GenEMQTT
  end

  test "the truth" do
    assert 1 + 1 == 2
  end
end

defmodule MqttToolsTest do
  use ExUnit.Case
  doctest MqttTools

  defmodule IntegrationTest do
    use MqttTools.GenEMQTT

    def start_link(pid, opts \\ []) do
      MqttTools.GenEMQTT.start_link(__MODULE__, pid, opts)
    end

    def on_connect(state) do
      send state, :connected
      {:ok, state}
    end

    def on_disconnect(state) do
      send state, :disconnected
      {:ok, state}
    end

    def on_publish(topic, message, state) do
      send state, {:published, topic, message}
      {:ok, state}
    end

    def on_subscribe(subscription, state) do
      send state, {:subscribed, subscription}
      {:ok, state}
    end

    def on_unsubscribe(subscription, state) do
      send state, {:unsubscribed, subscription}
      {:ok, state}
    end
  end

  test "should be able to link a process" do
    assert {:ok, _pid} = IntegrationTest.start_link(self)
  end

  test "should return already started if a named process has been started" do
    assert {:ok, pid} = IntegrationTest.start_link(self, name: MyTestName)
    assert {:error, {:already_started, ^pid}} = IntegrationTest.start_link(self, name: MyTestName)
  end

  test "should be able to connect" do
    {:ok, _pid} = IntegrationTest.start_link(self)
    assert_receive :connected
  end

  test "subscribe and then publish" do
    {:ok, pid} = IntegrationTest.start_link(self)
    assert_receive :connected

    assert :ok = MqttTools.GenEMQTT.subscribe(pid, "foo", 0)
    assert_receive {:subscribed, [{"foo", 0}]}

    assert :ok = MqttTools.GenEMQTT.publish(pid, "foo", "foo bar baz!", 0)
    assert_receive {:published, ["foo"], "foo bar baz!"}
  end

  test "subscribe and then unsubscribe" do
    {:ok, pid} = IntegrationTest.start_link(self)
    assert_receive :connected

    assert :ok = MqttTools.GenEMQTT.subscribe(pid, "foo", 0)
    assert_receive {:subscribed, [{"foo", 0}]}

    assert :ok = MqttTools.GenEMQTT.unsubscribe(pid, "foo")
    assert_receive {:unsubscribed, [["foo"]]}
  end

  test "subscribe and then unsubscribe to multiple topics" do
    {:ok, pid} = IntegrationTest.start_link(self)
    assert_receive :connected

    assert :ok = MqttTools.GenEMQTT.subscribe(pid, [{"foo", 0}, {"bar", 1}])
    assert_receive {:subscribed, [{"foo", 0}, {"bar", 1}]}

    assert :ok = MqttTools.GenEMQTT.unsubscribe(pid, "bar")
    assert_receive {:unsubscribed, [["bar"]]}

    assert :ok = MqttTools.GenEMQTT.unsubscribe(pid, "foo")
    assert_receive {:unsubscribed, [["foo"]]}
  end
end

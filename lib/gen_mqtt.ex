defmodule GenMQTT do
  # these follows the gen_server specs ---------------------------------
  @callback init(state) ::
    {:ok, state} |
    {:ok, state, timeout | :hibernate} |
    :ignore |
    {:stop, reason :: any} when state: any

  @type from :: {pid, tag :: term}
  @callback handle_call(request :: term, from, state) ::
    {:reply, reply, new_state} |
    {:reply, reply, new_state, timeout | :hibernate} |
    {:noreply, new_state} |
    {:noreply, new_state, timeout | :hibernate} |
    {:stop, reason, reply, new_state} |
    {:stop, reason, new_state} when reply: term, state: term, new_state: term, reason: term

  @callback handle_cast(request :: term, state) ::
    {:noreply, new_state} |
    {:noreply, new_state, timeout | :hibernate} |
    {:stop, reason :: term, new_state} when state: term, new_state: term

  @callback handle_info(msg :: :timeout | term, state) ::
    {:noreply, new_state} |
    {:noreply, new_state, timeout | :hibernate} |
    {:stop, reason :: term, new_state} when state: term, new_state: term

  @callback terminate(reason, state) ::
    term when state: term, reason: :normal | :shutdown | {:shutdown, term} | term

  @callback code_change(old_vsn, state :: term, extra :: term) ::
    {:ok, new_state :: term} |
    {:error, reason :: term} when old_vsn: term | {:down, term}

  # gen_emqtt ----------------------------------------------------------
  @type topic :: [binary] | binary
  @type qos :: 0 | 1 | 2

  @callback on_connect(state) ::
    {:ok, state} when state: term

  @callback on_connect_error(reason, state) ::
    {:ok, state} when [state: term,
                       reason: :server_not_found |
                               :server_not_available |
                               :wrong_protocol_version |
                               :invalid_id |
                               :invalid_credentials |
                               :not_authorized]

  @callback on_disconnect(state) ::
    {:ok, state} when state: term

  @callback on_subscribe([{topic, qos}], state) ::
    {:ok, state} when state: term

  @callback on_unsubscribe(topic, state) ::
    {:ok, state} when state: term

  @callback on_publish(topic, payload :: binary, state) ::
    {:ok, state} when state: term

  defmacro __using__(_) do
    quote location: :keep do
      @behaviour :gen_emqtt

      @doc false
      def init(state) do
        {:ok, state}
      end

      @doc false
      def on_connect(state) do
        {:ok, state}
      end

      @doc false
      def on_connect_error(reason, state) do
        {:ok, state}
      end

      @doc false
      def on_disconnect(state) do
        {:ok, state}
      end

      @doc false
      def on_subscribe([{_topic, _qos}]=subscription, state) do
        {:ok, state}
      end

      @doc false
      def on_unsubscribe([_topic], state) do
        {:ok, state}
      end

      @doc false
      def on_publish(_topic, _msg, state) do
        {:ok, state}
      end

      @doc false
      def handle_call(msg, _from, state) do
        # We do this to trick Dialyzer to not complain about non-local returns.
        reason = {:bad_call, msg}
        case :erlang.phash2(1, 1) do
          0 -> exit(reason)
          1 -> {:stop, reason, state}
        end
      end

      @doc false
      def handle_cast(msg, state) do
        # We do this to trick Dialyzer to not complain about non-local returns.
        reason = {:bad_cast, msg}
        case :erlang.phash2(1, 1) do
          0 -> exit(reason)
          1 -> {:stop, reason, state}
        end
      end

      @doc false
      def handle_info(_msg, state) do
        {:noreply, state}
      end

      @doc false
      def terminate(_reason, _state) do
        :ok
      end

      @doc false
      def code_change(_old_version, state, _extra) do
        {:ok, state}
      end

      defoverridable [
        init: 1,

        on_connect: 1, on_connect_error: 2, on_disconnect: 1,
        on_subscribe: 2, on_unsubscribe: 2,
        on_publish: 3,

        handle_call: 3, handle_cast: 2, handle_info: 2,
        terminate: 2, code_change: 3
      ]
    end
  end

  @typedoc "Return values of `start*` functions"
  @type on_start ::
    {:ok, pid} |
    :ignore |
    {:error, {:already_started, pid} | term}

  @typedoc "Debug options supported by the `start*` functions"
  @type debug :: [:trace | :log | :statistics | {:log_to_file, Path.t}]

  @typedoc "The GenEMQTT name"
  @type name :: atom | {:global, term} | {:via, module, term}

  @typedoc "Option values used by the `start*` functions"
  @type option :: {:debug, debug} |
                  {:name, name} |
                  {:timeout, timeout} |
                  {:spawn_opt, Process.spawn_opt}

  @type options :: [option]

  @doc """
  Start a linked connection to a MQTT broker
  """
  @spec start_link(module, any, options) :: on_start
  def start_link(module, args, options \\ []) when is_atom(module) and is_list(options) do
    case Keyword.pop(options, :name) do
      {nil, opts} ->
        :gen_emqtt.start_link(module, args, opts)
      {name, opts} when is_atom(name) ->
        :gen_emqtt.start_link({:local, name}, module, args, opts)
      # todo, check if this works with process registries like gproc
      {other, opts} when is_tuple(other) ->
        :gen_emqtt.start_link(other, module, args, opts)
    end
  end

  @doc """
  Start an unlinked connection to a MQTT broker
  """
  @spec start(module, any, options) :: on_start
  def start(module, args, options \\ []) when is_atom(module) and is_list(options) do
    case Keyword.pop(options, :name) do
      {nil, opts} ->
        :gen_emqtt.start(module, args, opts)
      {name, opts} when is_atom(name) ->
        :gen_emqtt.start({:local, name}, module, args, opts)
      # todo, check if this works with process registries like gproc
      {other, opts} when is_tuple(other) ->
        :gen_emqtt.start(other, module, args, opts)
    end
  end

  @doc """
  Subscribe to one or multiple topics given a list of tuples containing
  the topic name and its quality of service `[{"topic", 0}, ..]`
  """
  @spec subscribe(pid, [{topic :: binary, qos}]) :: :ok
  defdelegate subscribe(pid, topics), to: :gen_emqtt

  @doc """
  Subscribe to `topic` with quality of service set to `qos`
  """
  @spec subscribe(pid, topic, qos) :: :ok
  defdelegate subscribe(pid, topic, qos), to: :gen_emqtt

  @doc """
  Unsubscribe from one or more `topic`
  """
  @spec unsubscribe(pid, topic) :: :ok
  defdelegate unsubscribe(pid, topic), to: :gen_emqtt

  @doc """
  Publish `payload` to `topic` with quality of service set to `qos`
  """
  @spec publish(pid, topic, payload :: binary, qos) :: :ok
  def publish(pid, topic, payload, qos) when is_list(topic) do
    :gen_emqtt.publish(pid, topic, payload, qos)
  end
  def publish(pid, topic, payload, qos) when is_binary(topic) do
    publish(pid, [topic], payload, qos)
  end

  @doc """
  Make a call to the underlying state machine
  """
  @spec call(pid, request :: term) :: term
  defdelegate call(pid, request), to: :gen_emqtt

  @doc """
  Make a cast to the underlying state machine
  """
  @spec cast(pid, request :: term) :: :ok
  defdelegate cast(pid, request), to: :gen_emqtt
end

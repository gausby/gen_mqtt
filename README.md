# GenMQTT

An Elixir behaviour that makes it possible to communicate with a MQTT broker.

## Installation

This project is not available on hex as of yet. It can be installed by adding the following to the project mix-file:

``` elixir
  defp deps do
    [{:gen_mqtt, github: "gausby/mqtt_tools"}]
  end
```

## License

Work in progress.

This project rely on work done by [Erlio GmbH Basel Switzerland](http://erl.io), in specific the dependency [vmq_commons](https://github.com/erlio/vmq_commons/) which is released under an [Apache 2.0](https://github.com/erlio/vmq_commons/blob/master/LICENSE.txt)-license. I (Martin Gausby) claim no copyright or ownership to any of this.

Please notice that this is not a project run or owned by Erlio, so all support requests should be raised as [issues on the projects GitHub page](https://github.com/gausby/mqtt_tools/issues).

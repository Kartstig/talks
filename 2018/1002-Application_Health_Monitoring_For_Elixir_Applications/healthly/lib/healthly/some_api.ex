defmodule Healthly.SomeApi do
  use GenServer

  def start_link(args) do
    GenServer.start_link(__MODULE__, [args], name: __MODULE__)
  end

  @impl true
  def init(state) do
    {:ok, state}
  end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end

  def stop() do
    GenServer.stop(__MODULE__, :normal, :infinity)
  end
end
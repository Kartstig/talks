defmodule Kvagent do
  @cache_bucket :kvagent

  def start() do
    :ok
  end

  def init() do
    {:ok, pid} = Agent.start_link(fn -> %{} end, name: @cache_bucket)
    :io.format("Agent booted up on ~w~n", [pid])
    :io.format("Using Cache Store: ~w~n", [@cache_bucket])
  end

  def ffactorial(0), do: 1
  def ffactorial(number) do
    case fetch(number) do
      # Cache miss
      :nil ->
        :io.format("Cache miss: ~w~n", [number])
        result = number * ffactorial(number-1)
        store(number, result)
        result
      # Cache hit
      value ->
        :io.format("Cache Hit: ~w~n", [number])
        value
    end
  end

  defp fetch(number) do
    Agent.get(@cache_bucket, &Map.get(&1, number))
  end

  defp store(number, result) do
    Agent.update(@cache_bucket, &Map.put(&1, number, result))
  end
end

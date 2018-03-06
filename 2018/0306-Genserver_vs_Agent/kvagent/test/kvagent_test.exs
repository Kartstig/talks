defmodule KvagentTest do
  use ExUnit.Case
  doctest Kvagent

  test "greets the world" do
    assert Kvagent.hello() == :world
  end
end

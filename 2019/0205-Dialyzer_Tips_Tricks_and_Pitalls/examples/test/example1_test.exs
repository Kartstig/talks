defmodule Example1Test do
  use ExUnit.Case
  doctest Example1

  test "greets the world" do
    assert Example1.hello() == :world
  end
end

defmodule Example1 do
  @moduledoc """
  You don't even need to use typespecs
  to get errors from dialyzer
  """

  @sessions %{
    user1: DateTime.utc_now(),
    user2: DateTime.utc_now()
  }

  def has_session(user) do
    case Map.get(@sessions, user) do
      some_user -> true
      nil -> false
    end
  end
end

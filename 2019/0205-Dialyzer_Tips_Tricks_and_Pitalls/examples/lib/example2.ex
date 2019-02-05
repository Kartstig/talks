defmodule KitHub do
  @type cat() :: %{
          __cat__: String.t(),
          hunger: non_neg_integer(),
          lives: non_neg_integer()
        }

  @spec rename_cat(cat(), String.t()) :: cat()
  def rename_cat(%{__cat__: _shirley} = cat_facts, new_name) do
    %{cat_facts | __cat__: new_name}
  end

  @spec respawn(cat()) :: cat() | :dead
  def respawn(%{__cat__: _, lives: lives} = cat)
      when lives > 0 do
    %{cat | lives: lives - 1}
  end

  def respawn(%{__cat__: _, lives: lives})
      when lives == 0,
      do: :dead

  @spec feed(cat()) :: cat()
  def feed(%{__cat__: _, hunger: hunger} = cat) do
    new_hunger =
      case hunger - 20 do
        h when h < 0 -> 0
        h -> h
      end

    %{cat | hunger: new_hunger}
  end

  @spec play(cat()) :: cat()
  def play(%{__cat__: _, hunger: hunger} = cat) do
    case hunger + 60 do
      h when h > 100 -> KitHub.respawn(cat)
      h -> %{cat | hunger: h}
    end
  end

  @spec generate_cat(String.t()) :: cat()
  def generate_cat(name) when is_binary(name) do
    %{
      __cat__: name,
      lives: 9
    }
  end
end

defmodule KittenGenerator do
  def start() do
    KitHub.generate_cat(:lucy)
  end
end

defmodule KittenServer do
  use GenServer

  @names ["Bob Meowerly", "Lucifurr", "The Great Catsby"]

  def init() do
    cats = for name <- @names, do: KitHub.generate_cat(name)
    {:ok, cats}
  end

  def handle_cast({:feed}, cats) do
    idx = Enum.random(0..(length(cats) - 1))

    {cat_to_feed, others} =
      cats
      |> List.pop_at(idx)

    fed_cat_that_still_complains =
      cat_to_feed
      |> KitHub.feed()

    {:noreply, [others | fed_cat_that_still_complains]}
  end

  # def handle_cast({:play}, cats) do
  #   idx = Enum.random(1..length(cats))
  #   hungry_kitty =
  #     cats
  #     |> List.pop_at()
  #     |> KitHub.feed()

  #   case hungry_kitty do
  #     :dead -> {:noreply, cats}
  #   end
  # end
end

defmodule HealthlyWeb.ServiceController do
  use HealthlyWeb, :controller

  def action(%{query_params: %{"name" => name, "action" => action}} = conn, _params) do
    mod = String.to_existing_atom("Elixir." <> name)

    case action do
      "START" ->
        apply(mod, :start_link, [%{}])

      "STOP" ->
        apply(mod, :stop, [])
    end
    text(conn, "OK")
  end
end

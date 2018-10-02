# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

config :ex_health,
  module: Healthly.HealthChecks,
  interval_ms: 50

# General application configuration
config :healthly,
  ecto_repos: [Healthly.Repo]

# Configures the endpoint
config :healthly, HealthlyWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "6K+qdW5vfhRBdSrDpT18/rwVPdy/erGRcTUPoGUmk65IFuRD//Ml0k+19MWQ4zE8",
  render_errors: [view: HealthlyWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Healthly.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:user_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"

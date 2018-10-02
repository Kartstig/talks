defmodule Healthly.HealthChecks do
  import ExHealth

  process_check(Healthly.AuthServer)
  process_check(Healthly.SomeApi)
  process_check(Healthly.MailServer)
end
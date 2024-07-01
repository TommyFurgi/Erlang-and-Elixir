defmodule Pollutiondb.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Starts a worker by calling: Pollutiondb.Worker.start_link(arg)
      # {Pollutiondb.Worker, arg}
      Pollutiondb.Repo
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Pollutiondb.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def add_stations() do
    stations = [
      %Pollutiondb.Station{name: "s1", lon: 1.1, lat: 1.1},
      %Pollutiondb.Station{name: "s2", lon: 2.1, lat: 2.1},
      %Pollutiondb.Station{name: "s3", lon: 3.1, lat: 3.1},
      %Pollutiondb.Station{name: "s4", lon: 4.1, lat: 4.1}
    ]

    station_data = Enum.map(stations, &Map.from_struct/1)
    Pollutiondb.Repo.insert_all(Pollutiondb.Station, station_data)
  end
end

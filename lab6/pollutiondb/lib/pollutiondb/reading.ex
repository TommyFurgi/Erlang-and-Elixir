defmodule Pollutiondb.Reading do
  use Ecto.Schema
  require Ecto.Query

  schema "readings" do
    field :datetime, :utc_datetime
    field :type, :string
    field :value, :float

    belongs_to :station, Pollutiondb.Station
  end

  def insert_mock_data() do
    [
      {"Station1", "PM10", 121.5},
      {"Station2", "PM2.5", 63.5},
      {"s1", "PM10", 21.5},
      {"s2", "PM10", 28.5},
    ]
    |> Enum.map(fn {station_name, type, val} ->
      add_now(station_name |> Pollutiondb.Station.find_by_name(), type, val) end)

    [
      {"s1", "PM10", 17.5}
    ]
    |> Enum.map(fn {station_name, type, val} ->
      add(%Pollutiondb.Reading{
        station: station_name |> Pollutiondb.Station.find_by_name(),
        type: type, value: val,
        datetime: ~U[2023-06-20 23:11:50Z]
      }) end)
  end

  def get_all() do
    Pollutiondb.Repo.all(Pollutiondb.Reading)
    |> Pollutiondb.Repo.preload(:station)
  end

  def add(reading) do
    Pollutiondb.Repo.insert(reading)
  end

  def add_now(station, type, value) do
    add(%Pollutiondb.Reading{
      station: station, type: type, value: value,
      datetime: DateTime.utc_now |> DateTime.truncate(:second)
    })
  end

  def find_by_date(date) do
    minDateTime = DateTime.new!(date, ~T[00:00:00])
    maxDateTime = DateTime.add(minDateTime, 24*60*60, :second)
    Ecto.Query.from(
      r in Pollutiondb.Reading,
      where: ^minDateTime <= r.datetime,
      where: r.datetime <= ^maxDateTime
    )
    |> Pollutiondb.Repo.all
  end
end

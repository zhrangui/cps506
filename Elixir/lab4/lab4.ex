defmodule EagerLazy do
  data = [1, 2, 5, 3, 9, 10, 25, 7, 44, 30]

  # Enum eager evaluation
  eager_max = Enum.max(data)
  IO.inspect(eager_max)
  eager_sort = Enum.sort(data)
  IO.inspect(eager_sort)

  # lazy sum and filter list by stream
  lazy_sum = Stream.map(data, &(&1*&1)) |> Enum.sum
  IO.inspect(lazy_sum)
  odd = data |> Stream.map(&(&1 * 3)) |> Stream.filter(&(rem(&1, 2) != 0))
  IO.inspect(odd)
end



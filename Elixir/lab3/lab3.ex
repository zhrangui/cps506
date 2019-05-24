defmodule Lab3 do
  def sum(list) do
    List.foldr(list, 0, &(&1/5 + &2))
  end
end

data = [1, 2, 5, 3, 9, 10, 25, 7, 44, 30]
IO.puts(Lab3.sum(data))

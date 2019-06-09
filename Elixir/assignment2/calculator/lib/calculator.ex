defmodule Calculator do
  @moduledoc """
  Documentation for Calculator.
  """
  def add(a, b) do
    a + b
  end

  def sub(a, b) do
    a - b
  end

  def multiply(a, b) do
    a * b
  end

  def divide(a, b) do
    a / b
  end

  def sin(x) do
    :math.sin(x)
  end

  def cos(x) do
    :math.cos(x)
  end
end

defmodule CalculatorTest do
  use ExUnit.Case
  doctest Calculator

  test "add" do
    assert Calculator.add(2, 3) == 5
  end

  test "sub" do
    assert Calculator.sub(5, 3) == 2
  end

  test "multiply" do
    assert Calculator.multiply(2, 3) == 6
  end

  test "divide" do
    assert Calculator.divide(6, 3) == 2
  end

  test "sin" do
    assert Calculator.sin(:math.pi/2) == 1
  end

  test "cos" do
    assert Calculator.cos(0) == 1
  end
end

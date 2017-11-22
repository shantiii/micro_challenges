defmodule BootstrappedSkewBinomialHeap do
  def new do
    []
  end

  def merge([], h), do: h
  def merge(h, []), do: h
  def merge({x, p1} = h1, {y, p2} = h2) do
    if x <= y do
      {x, SkewBinomialHeap.insert(p1, h2)}
    else
      {y, SkewBinomialHeap.insert(p2, h1)}
    end
  end

  def insert(heap, x) do
    merge({x, []}, heap)
  end

  def peek([]), do: raise "heap is empty"
  def peek({x, _}), do: x

  def drop([]), do: raise "heap is empty"
  def drop({_, []}), do: []
  def drop({_, p}) do
    {y, p_1} = SkewBinomialHeap.peek(p)
    p_2 = SkewBinomialHeap.drop(p)
    {y, SkewBinomialHeap.merge(p_1, p_2)}
  end
end


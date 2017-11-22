defmodule SkewBinomialHeap do
  def new do
    []
  end

  defp rank({rank, _x, _xs, _c}), do: rank

  defp root({_rank, x, _xs, _c}), do: x

  defp link({rank_x, x, xs, c_x} = node_x, {rank_y, y, ys, c_y} = node_y) do
    if x <= y do
      {rank_x + 1, x, xs, [node_y | c_x]}
    else
      {rank_x + 1, y, ys, [node_x | c_y]}
    end
  end

  defp skew_link(x, node_a, node_b) do
    {r, y, ys, c_y} = link(node_a, node_b)
    if x <= y do
      {r, x, [y | ys], c_y}
    else
      {r, y, [x | ys], c_y}
    end
  end
  
  defp insert_tree([], tree), do: [tree]
  defp insert_tree([t | ts], tree) do
    if tree <= t do
      [tree, t | ts]
    else
      insert_tree(ts, link(tree, t))
    end
  end

  defp merge_trees([], ts), do: ts
  defp merge_trees(ts, []), do: ts
  defp merge_trees([x | xs], [y | ys]) do
    cond do
      rank(x) < rank(y) ->
        [x | merge_trees(xs, [y | ys])]
      rank(x) > rank(y) ->
        [y | merge_trees([x | xs], ys)]
      true ->
        insert_tree(merge_trees(xs, ys), link(x, y))
    end
  end

  defp normalize([]), do: []
  defp normalize([t | ts]), do: insert_tree(ts, t)

  def insert([{r, _, _, _} = t_1, {r, _, _, _} = t_2 | ts], x) do
    [skew_link(x, t_1, t_2) | ts]
  end
  def insert(heap, x) do
    [{0, x, [], []} | heap]
  end

  def merge(heap_a, heap_b) do
    merge_trees(normalize(heap_a), normalize(heap_b))
  end

  def peek([]), do: raise "empty heap"
  def peek([t]), do: root(t)
  def peek([t | ts]), do: min(root(t), peek(ts))

  defp get_min_tree([t]), do: {t, []}
  defp get_min_tree([t | ts]) do
    {x, xs} = get_min_tree(ts)
    if root(t) < root(x) do
      {t, ts}
    else
      {x, [t | xs]}
    end
  end

  def drop([]), do: raise "empty heap"
  def drop(trees) do
    {{_, x, xs, c}, ts} = get_min_tree(trees)
    Enum.reduce(xs, merge_trees(normalize(ts), Enum.reverse(c)), &insert(&2, &1))
  end
end


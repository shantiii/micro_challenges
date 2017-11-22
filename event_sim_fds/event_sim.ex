defmodule EventQueue do
  def new do
    []
  end
  def invalidate(queue, udpate) do
  end
end

defmodule RealTimeQueue do
  def new do
    {nil, [], nil}
  end

  def enqueue(q, x) do
  end
  def head(q) do
  end
  def tail(q) do
  end

  defp rotate(nil, r, a) do
    [stream_head(r) | a]
  end
  defp rotate(f, r, a) do
    [stream_head(f) | rotate( 
  end

  defp stream_cons(s, x) do
    Stream.concat([[x], s])
  end
  defp stream_head(s) do
    hd(Enum.take(s, 1))
  end
  defp stream_tail(s) do
    tail = Stream.drop(s, 1)
    if is_nil(Enum.at(tail, 0)) do
      nil
    else
      tail
    end
  end

end

defmodule EventSim do
  require Record
  Record.defrecord :position, [x: 0, y: 0]
  Record.defrecord :velocity, [dx: 0, dy: 0]
  def new do
  end

  def step(state, time) do
  end
end

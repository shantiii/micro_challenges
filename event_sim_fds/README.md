# "Event Simulation" and Functional Data Structures in Elixir

This started off as a crack at making an event simulation engine that could
work in realtime to calculate the results of interactions by updating objects
through time.

Maintaining an ordered sequence of events in time requires the presence of a
priority queue, and this ended up being a good place for me to implement a
Bootstrapped Skew-Binomial Heap, which is an implementation of a priority queue
that has O(1) for most operations and O(log(N)) for delete-min. And it's all
defined in elixir, so that's nifty.

This is a candidate for my eventual functional data structures library.

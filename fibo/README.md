# Fibonacci Sequences

This project was me avoiding cleaning my apartment in prep for Thanksgiving. It
also gave me an opportunity to work with memoization in dynamic programming
problems, as implemented in Erlang.

After the stupid implementation of Fibo numbers, I discovered [from
Wikipedia](http://en.wikipedia.org/Fibonacci_number) that there was a
better-converging solution to getting the Nth Fibonacci number, skipping
calculating a large amount of the in-between numbers.

I implemented that in Erlang, using a little bit of hairy memoization, and got
a pretty reasonable compute speed. I switched from using a map
(`fibo:fibonacci/1`) to using an ETS table (`fibo:fibonacci2/1`), but there
doesn't seem to be an appreciable difference in speed. On my 2011 Macbook Air,
I can comfortably calculate the millionth Fibonacci number in less than a
second.

```text
Eshell V9.0.5  (abort with ^G)
1> c(fibo), element(1, timer:tc(fibo, fibonacci, [1000000])).
358617
2> c(fibo), element(1, timer:tc(fibo, fibonacci2, [1000000])).
372638
```

Erlang/OTP's implementation of big-number arithmetic doesn't leverage GNU
Multiple Precision library, but it could, based on the fact that there is some
GMP code currently included in the OTP release. So, that's a potential place
for improvement. Curious as to _how much_ improvement GMP would offer, I tried
to cobble together a solution in C.

My efforts in C ended up being attempts to pull together the GMP library with
existing data structures code (`sys/rbtree.h`, here's to you). I/O of the
numbers quickly became the bottleneck, after fixing a couple of memory bounding
issues. That's why there is a `-q` flag available to make it quiet.

`fibo.c` can be built with the following command:
```bash
gcc -std=c99 -Wall -Wextra -pedantic -O3 -l gmp -o fibo fibo.c
```

```text
$ time ./fibo --smart --quiet 10000000
./fibo --smart --quiet 10000000  0.25s user 0.01s system 97% cpu 0.269 total
```

Running the command is self-explanatory, but you probably want to pass the
`--smart` option if you do any reasonably large number N. This C implementation
appears to be around 10 times faster as the Erlang version, but seems to scale
similarly (linearly/linearithmically with respect to N).

Other possible grounds for exploration are the repeated-2x2 matrix
exponentiation space (move the computation to the GPU?) and the repeated
exponentiation of a rational term.

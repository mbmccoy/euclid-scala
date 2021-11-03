# Euclidean and related algorithms in Scala / Dotty

A sampler of Euclidean algorithms for `BigInt`s and polynomials, written in Scala 3. 


## `BigInt` algorithms

These purely-functional methods provide textbook implementations of 
number theory primitives, designed to be read by humans. The source appears in 
[src/main/scala/com/mbmccoy/Euclid.scala](./src/main/scala/com/mbmccoy/Euclid.scala).

  - `gcd` and `lcm` implement the Euclidean greatest common divisor and least 
    common multiple algorithms.

  - `bezout` generates [Bézout numbers][bezout] via the extended Euclidean 
    algorithm.

  - `modDiv` computes modular division: Given integers `a`, `n`, and `d`, the 
    `modDiv` function returns a number `m` such that `m * d == a (mod n)`, 
    i.e., it divides `a` by `d`  modulo `n`.

  - `continuedFraction` streams a continued fraction expansion of two integers. 

  - `convergents` returns a stream of [convergents][convergents] 
    for a ratio `n / d`.

  - `rationalApprox` provides the best rational approximation to a fraction,
    given an upper-bound on the denominator.

Effort has been made to make these algorithms correct, concise, and idiomatic 
Scala. If you have ideas for improvements, we welcome pull requests that 
further these goals. 


## Polynomial algorithms

We have implemented a number of polynomial and finite field algorithms. These appear in the `com.mbmccoy.euclid.polynomial` package.


## Building and testing

Tests for the functions can be run via `sbt test`. To create a fat jar, use `sbt assembly`.


### Notebooks

You can run a scala3 notebook (assuming jupyter is installed) using the
method described [here](https://github.com/almond-sh/almond/issues/718#issuecomment-941908746), to wit:

Make sure that `jupyter` is installed and available on your `PYTHONPATH`, e.g., activate a virtual environment.

Check out the `almond` repo and update the dependencies (only needs to be done once).
```
git clone git@github.com:almond-sh/almond.git
cd almond
git submodule init
git submodule update
```

Finally, run `mill` from the base of the `almond` directory, and point 
the jupyter server where you want the code to be run:
```
./mill -i jupyter 3.0.1 <path/to/root/folder>
```


## License

Software released under the MIT license. See the [LICENSE](./LICENSE) for 
details.


[bezout]: https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity
[convergents]: https://en.wikipedia.org/wiki/Continued_fraction#Infinite_continued_fractions_and_convergents
[dotty]: http://dotty.epfl.ch/
[dotty-docs]: http://dotty.epfl.ch/docs/
[scala-road-ahead]: https://www.slideshare.net/Odersky/scala-days-nyc-2016

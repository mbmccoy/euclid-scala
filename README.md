# Euclidean and related algorithms in Scala / Dotty

A sampler of Euclidean algorithms written in pure Scala / [Dotty][dotty]. 


## Algorithms

These purely-functional methods provide textbook implementations of 
number theory primitives, designed to be read by humans. The source appears in 
[src/main/scala/Euclid.scala](./src/main/scala/Euclid.scala).

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

All algorithms operate on `BigInt`s.


## Building and testing

Tests for the functions can be run via `sbt test`. The code compiles using the
[Dotty][dotty] compiler.


## Dotty, you say?

[Dotty][dotty] represents the future of Scala. While the two languages are not 
fully compatible, if you've programmed in Scala but haven't heard of Dotty, you 
probably won't notice any differences than a significantly faster build.

You can learn more about Dotty from [Dotty's documentation][dotty-docs], or 
just get an overview of the changes by perusing 
[Martin Odersky's slides][scala-road-ahead].


## License

Software released under the MIT license. See the [LICENSE](./LICENSE) for 
details.


[bezout]: https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity
[convergents]: https://en.wikipedia.org/wiki/Continued_fraction#Infinite_continued_fractions_and_convergents
[dotty]: http://dotty.epfl.ch/
[dotty-docs]: http://dotty.epfl.ch/docs/
[scala-road-ahead]: https://www.slideshare.net/Odersky/scala-days-nyc-2016

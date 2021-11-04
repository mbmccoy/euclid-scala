package com.mbmccoy.euclid.polynomial

import algebra.ring.Field

object FieldOps {
    /**
     * Invert all field elements.
     *
     * This implementation uses Montgomery batch inversion, which requires
     * only a single inverse computation and 3 * n multiplications.
     *
     */
    def batchInverse[T](elements: List[T])(implicit ev: Field[T]): List[T] = {
        val clean = elements.view.filter(_ != ev.zero)
        // Sequential products [1, a, ab, abc, ..., ab..n]
        val products = clean.scan(ev.one)(ev.times(_, _))
        // Compute single inverse 1/ab..n
        val finalInverse = ev.reciprocal(products.last) 
        // Computes the inverse of products [1, 1/a, 1/ab, ..., 1/ab..n]
        val inverses = clean.scanRight(finalInverse)(ev.times(_, _))
        // Finally, compute [1 * 1/a, a * 1/ab, ..., ab..(n-1) * 1/ab..n]
        inverses.tail.zip(products).map(ev.times(_, _)).toList
    }

    /**
     * Get monic polynomial with given roots
     */
    def rootPolynomial[T](points: List[T])(implicit ev: Field[T]): Polynomial[T] = {
        points
          .foldLeft(Polynomial.one[T])((acc, point) => {
            acc * Polynomial((0, ev.negate(point)), (1, ev.one))
        })
    }

    /**
     * Find polynomial that goes through a list of (x, y) coordinates.
     */
    def lagrangeInterpolation[T](points: List[(T, T)])(implicit ev: Field[T]): Polynomial[T] = {
        // The product of (x - x_i) for all i
        val rootPoly = rootPolynomial(points.map{p => p._1})
        // TODO: These could be a lot more efficient. E.g., with evaluation trees
        //
        //                  (a * b * c * d)
        //               (a*b)           (c*d)
        //              (a) (b)         (c)  (d)
        //
        // Then (a*b*d) = (a*b) * d . Basically, do O(N log(N)) multiplications,
        // to build the tree, then do O(log(N)) operations to evaluate each 
        // numerator below, for a total of O(N log(N)) multiplications.
        // Then apply batch inversion to get the denominators.
        points
            .map(point => {
                // Evaluate the polynomial
                val numeratorPoly: Polynomial[T] = rootPoly/Polynomial((0, ev.negate(point._1)), (1, ev.one))
                val denominator: T = numeratorPoly(point._1)
                val numerator: Polynomial[T] = point._2 * numeratorPoly
                numerator / denominator
            })
            .reduce(_ + _)
    }
}

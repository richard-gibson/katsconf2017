package katsconf

import cats.kernel._

object Exercise4 {

  object Matrix {

    def randomIntMatrix(size: Int): Matrix[Int] =
      Matrix(List.fill(size)(List.fill(size)(scala.util.Random.nextInt(20) - 10)))

  }

  case class Matrix[A](entries: List[List[A]]) {
    val size = entries.length

    require(entries.forall(_.length == size))

    override def toString: String =
      entries.map(row =>
        row.mkString("(", " ", ")")
      ).mkString("\n")

    // Task 1: Implement matrix addition.
    // (Assume that both matrices have the same size)

    def +(that: Matrix[A])(implicit sg: Semigroup[A]): Matrix[A] = {
      require(this.size == that.size)
      ???
    }

    // Task 2: Implement scalar multiplication.
    // (Multiply each entry with a given scalar value)

    def scale(r: A)(implicit sg: Semigroup[A]): Matrix[A] =
      ???
  }

  // Task 3: Notice anything fishy?
  // Look at the way the operations from the `Semigroup` are used.
  //
  // Hints:
  //   - How would you implement the unit matrix?
  //
  //     (1 0 0 0)
  //     (0 1 0 0)
  //     (0 0 1 0)
  //     (0 0 0 1)
  //
  //   - Can you implement matrix multiplication?

}

package katsconf

import cats.data._
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}

object Exercise1A {

  // Task 2: Fetch the data as previously. Don't use nested for-comprehensions.

  def mkEmployeeAsync(name: String, zipCode: String, city: String, salary: Int)
                     (implicit ec: ExecutionContext): Future[Either[Failure, Employee]] =
    (for {
      n <- EitherT(nonBlank("name")(name))
      z <- EitherT(validZip(zipCode))
      c <- EitherT(validCity(city))
      s <- EitherT(inRange("salary", 0, 100000)(salary))
    } yield Employee(n, z, c, s)).value

  def nonBlank(name: String)(data:String)
              (implicit ec: ExecutionContext): Future[Either[Failure, String]] =
    EitherT.right(Future(data)).
      ensure(EmptyString(s"$name cannot be blank"))(_.nonEmpty).value

  def inRange(name: String, lower: Int, upper: Int)(data:Int)
             (implicit ec: ExecutionContext): Future[Either[Failure, Int]] =
    EitherT.right(Future(data)).
      ensure(ValueOutOfRange(data))(i => i >=lower && i <=upper).value

  def validZip(data: String)
              (implicit ec: ExecutionContext): Future[Either[Failure, String]] =
    EitherT.right(Future(data)).
      ensure(InvalidCity(data))(zip contains _).value

  def validCity(data: String)
               (implicit ec: ExecutionContext): Future[Either[Failure, String]] =
    EitherT.right(Future(data)).
      ensure(InvalidCity(data))(cities contains _).value

}

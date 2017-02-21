/**
  * Created by richardgibson on 21/02/2017.
  */
package object katsconf {
  sealed trait Failure
  case class EmptyString(fieldName:String) extends Failure
  case class InvalidCity(city:String) extends Failure
  case class ValueOutOfRange(i:Int) extends Failure
  case object UnknownEmployee extends Failure
  case object AccessDenied extends Failure

  final val zip = List("00111", "001122", "000333")
  final val cities = List("Dublin", "London", "Madrid")
  case class Employee private[katsconf](name: String, zipCode: String, city: String, salary: Int)

}

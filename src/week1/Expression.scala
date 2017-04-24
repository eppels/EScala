package week1

/**
 * Extended Exercise 1 - A calculator
 * Chose to implement eval using pattern matching because I thought it would be easier to see the implementtation all in one place instead of spread across the classes
 * Didn't use a separate object to implement because only using the data in the classes to implement the method
 */

sealed trait Expression {
  def eval : EvalResult = this match {
    case Number(value) => Success(value)
    case Addition(left, right) => (left.eval, right.eval) match {
      case (Failure(msg), _) => Failure(msg)
      case (_, Failure(msg)) => Failure(msg)
      case _ => Success(left.eval.result + right.eval.result)
    }
    case Subtraction(left, right) => (left.eval, right.eval) match {
      case (Failure(msg), _) => Failure(msg)
      case (_, Failure(msg)) => Failure(msg)
      case _ => Success(left.eval.result - right.eval.result)
    }
    case Division(numerator, denominator) => if (denominator.eval.result == 0) Failure("Division by zero") else (numerator.eval, denominator.eval) match {
      case (Failure(msg), _) => Failure(msg)
      case (_, Failure(msg)) => Failure(msg)
      case _ => Success(numerator.eval.result / denominator.eval.result)
    }
    case SquareRoot(value) => if (value.eval.result < 0) Failure("Square root of negative number") else value.eval match {
      case Failure(msg) => Failure(msg)
      case _ => Success(Math.sqrt(value.eval.result))
    }
  }
}

final case class Number(value: Double) extends Expression
final case class Addition(left: Expression, right: Expression) extends Expression
final case class Subtraction(left: Expression, right: Expression) extends Expression
final case class SquareRoot(value: Expression) extends Expression
final case class Division(numerator: Expression, denominator: Expression) extends Expression


sealed trait EvalResult {
  def result : Double
}

final case class Failure(reason: String) extends EvalResult {
  val result = Double.NaN
}

final case class Success(result: Double) extends EvalResult

object TestCalculator extends App {
  println(SquareRoot(Number(-1.0)).eval)
  println(Addition(SquareRoot(Number(-1.0)), Number(2.0)).eval)
  assert(Addition(SquareRoot(Number(4.0)), Number(2.0)).eval == Success(4.0))
  assert(Division(Number(4), Number(0)).eval == Failure("Division by zero"))
}
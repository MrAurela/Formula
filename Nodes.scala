package expressions


//SIMPLIFY ON OMA LISÄYS


/**
 * Error for missing variable bindings in evaluation.
 */
case class UnknownValueException(v: Variable) extends Exception(v.toString + " could not be bound.")

/**
 * An abstract expression node for exercise round 16, exercises 1,2,3,4.
 */
sealed trait Exp {
  def value(bindings: Map[Variable, Double]): Double
  def simplify: Exp
}

/**
 * A variable can get a value in evaluation, see `value` method.
 */
case class Variable(name: String) extends Exp {
  def value(bindings: Map[Variable, Double]) = bindings.getOrElse(this, throw new UnknownValueException(this))
  def simplify = this
}

/**
 * A constant is fixed to a numeric value.
 */
case class Const(c: Double) extends Exp {
  def value(bindings: Map[Variable, Double]) = c
  def simplify = this
}

/**
 * Addition joins two expression nodes into a sum.
 */
case class Add(u: Exp, v: Exp) extends Exp {
  def value(bindings: Map[Variable, Double]) = u.value(bindings) + v.value(bindings)
  def simplify = this match {
    case Add(Const(0), f: Exp) => f
    case Add(e: Exp, Const(0)) => e
    case Add(Const(c), Const(d)) => Const(this.value(Map()))
    case Add(e: Exp, f: Exp) => Add(e.simplify, f.simplify)
  }

}

/**
 * Multiplication joins two expression nodes into a product.
 */
case class Mul(u: Exp, v: Exp) extends Exp {
  def value(bindings: Map[Variable, Double]) = u.value(bindings) * v.value(bindings)
  def simplify = this match {
        case Mul(Const(0), f: Exp) => Const(0)
        case Mul(e: Exp, Const(0)) => Const(0)    
        case Mul(Const(1), f: Exp) => f
        case Mul(e: Exp, Const(1)) => e
        case Mul(Const(c), Const(d)) => Const(this.value(Map()))
        case Mul(e: Exp, f: Exp) => Mul(e.simplify, f.simplify)
  }

}

/**
 *  Provided as an example only - DO NOT use this in exercises.
 */
object Add {
  def apply(e: Exp*): Exp = e.toList match {
    case Nil => Const(0)
    case f :: Nil => f
    case f :: g => new Add(f, apply(g: _*))
  }
}

object Mul {
  def apply(e: Exp*): Exp = e.toList match {
    case Nil => Const(1)
    case f :: Nil => f
    case f :: g => new Mul(f, apply(g: _*))
  }
}

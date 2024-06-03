package taller4

sealed trait Expr
case class Numero(d: Double) extends Expr
case class Atomo(x: Char) extends Expr
case class Suma(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Resta(e1: Expr, e2: Expr) extends Expr
case class Div(e1: Expr, e2: Expr) extends Expr
case class Expo(e1: Expr, e2: Expr) extends Expr
case class Logaritmo(e1: Expr) extends Expr

class Newton {

  def eval(expr: Expr, vars: Map[Char, Double]): Double = expr match {
    case Numero(d) => d
    case Atomo(x) => vars(x)
    case Suma(e1, e2) => eval(e1, vars) + eval(e2, vars)
    case Prod(e1, e2) => eval(e1, vars) * eval(e2, vars)
    case Resta(e1, e2) => eval(e1, vars) - eval(e2, vars)
    case Div(e1, e2) => eval(e1, vars) / eval(e2, vars)
    case Expo(e1, e2) => Math.pow(eval(e1, vars), eval(e2, vars))
    case Logaritmo(e1) => Math.log(eval(e1, vars))
  }

  def derive(expr: Expr, variable: Char): Expr = expr match {
    case Numero(_) => Numero(0)
    case Atomo(x) if x == variable => Numero(1)
    case Atomo(_) => Numero(0)
    case Suma(e1, e2) => Suma(derive(e1, variable), derive(e2, variable))
    case Resta(e1, e2) => Resta(derive(e1, variable), derive(e2, variable))
    case Prod(e1, e2) => Suma(Prod(derive(e1, variable), e2), Prod(e1, derive(e2, variable)))
    case Div(e1, e2) => Div(Resta(Prod(derive(e1, variable), e2), Prod(e1, derive(e2, variable))), Prod(e2, e2))
    case Expo(e1, Numero(d)) => Prod(Prod(Numero(d), Expo(e1, Numero(d - 1))), derive(e1, variable))
    case Expo(e1, e2) => Prod(Expo(e1, e2), Suma(Prod(derive(e1, variable), Div(e2, e1)), Prod(Logaritmo(e1), derive(e2, variable))))
    case Logaritmo(e1) => Div(derive(e1, variable), e1)
  }

  def newton(expr: Expr, variable: Char, x0: Double, tolerance: Double = 1e-7, maxIter: Int = 1000): Option[Double] = {
    def iter(xi: Double, iterCount: Int): Option[Double] = {
      val fxi = eval(expr, Map(variable -> xi))
      if (Math.abs(fxi) < tolerance) {
        Some(xi)
      } else if (iterCount >= maxIter) {
        None
      } else {
        val dfxi = eval(derive(expr, variable), Map(variable -> xi))
        if (dfxi == 0) {
          None
        } else {
          val xi1 = xi - fxi / dfxi
          iter(xi1, iterCount + 1)
        }
      }
    }
    iter(x0, 0)
  }
}
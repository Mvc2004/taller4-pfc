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

  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(d) => d
    case Atomo(x) => if (x == a.x) v else throw new IllegalArgumentException(s"Átomo no asignado: $x")
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Div(e1, e2) =>
      val denom = evaluar(e2, a, v)
      if (denom == 0) throw new ArithmeticException("División por cero")
      evaluar(e1, a, v) / denom
    case Expo(e1, e2) => Math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) =>
      val arg = evaluar(e1, a, v)
      if (arg <= 0) throw new ArithmeticException("Logaritmo de número no positivo")
      Math.log(arg)
  }

  def derivar(f: Expr, a: Atomo): Expr = f match {
    case Numero(_) => Numero(0.0)
    case Atomo(x) => if (x == a.x) Numero(1.0) else Numero(0.0)
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Prod(e2, e2))
    case Expo(e1, e2) => Prod(Expo(e1, e2), Suma(Prod(derivar(e2, a), Logaritmo(e1)), Prod(e2, derivar(e1, a))))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)
  }

  def newton(f: Expr, a: Atomo, x0: Double, epsilon: Double, maxIter: Int = 10000000): Option[Double] = {
    def buenAprox(x: Double): Boolean = Math.abs(evaluar(f, a, x)) < epsilon

    var xi = x0
    var iter = 0

    while (!buenAprox(xi) && iter < maxIter) {
      try {
        xi = xi - evaluar(f, a, xi) / evaluar(derivar(f, a), a, xi)
      } catch {
        case _: ArithmeticException => return None
      }
      iter += 1
    }
    Some(xi).filter(buenAprox)
  }

  def mostrar(e: Expr): String = e match {
    case Numero(d) => d.toString
    case Atomo(x) => x.toString
    case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
    case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
    case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
    case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
    case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
    case Logaritmo(e1) => s"log(${mostrar(e1)})"
  }

  def limpiar(f: Expr): Expr = f match {
    case Suma(e1, Numero(0.0)) => limpiar(e1)
    case Suma(Numero(0.0), e2) => limpiar(e2)
    case Resta(e1, Numero(0.0)) => limpiar(e1)
    case Prod(e1, Numero(1.0)) => limpiar(e1)
    case Prod(Numero(1.0), e2) => limpiar(e2)
    case Prod(_, Numero(0.0)) => Numero(0.0)
    case Prod(Numero(0.0), _) => Numero(0.0)
    case Div(e1, Numero(1.0)) => limpiar(e1)
    case Expo(_, Numero(0.0)) => Numero(1.0)
    case Expo(e1, Numero(1.0)) => limpiar(e1)
    case Suma(e1, e2) => Suma(limpiar(e1), limpiar(e2))
    case Resta(e1, e2) => Resta(limpiar(e1), limpiar(e2))
    case Prod(e1, e2) => Prod(limpiar(e1), limpiar(e2))
    case Div(e1, e2) => Div(limpiar(e1), limpiar(e2))
    case Expo(e1, e2) => Expo(limpiar(e1), limpiar(e2))
    case Logaritmo(e1) => Logaritmo(limpiar(e1))
    case _ => f
  }
}

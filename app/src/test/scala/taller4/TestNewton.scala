package taller4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NewtonSpec extends AnyFlatSpec with Matchers {

  val newtonSolver = new Newton()

  "Newton's method" should "find the root of a simple linear function" in {
    val expr = Resta(Atomo('x'), Numero(5.0))  // f(x) = x - 5
    val root = newtonSolver.newton(expr, 'x', 0.0)
    root shouldBe Some(5.0)
  }

  it should "find the root of a quadratic function" in {
    val expr = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))  // f(x) = x^2 - 4
    val root = newtonSolver.newton(expr, 'x', 3.0)
    root shouldBe Some(2.0)
  }

  it should "find the root of a more complex function" in {
    val expr = Suma(
      Prod(Numero(5.0), Atomo('k')),
      Div(Logaritmo(Numero(3.0)), Expo(Resta(Numero(8.0), Atomo('x')), Atomo('x')))
    )
    val root = newtonSolver.newton(expr, 'x', 1.0)
    root should not be empty  // This is a more complex function, ensure it finds some root
  }

  it should "return None if no root is found within the maximum number of iterations" in {
    val expr = Resta(Prod(Atomo('x'), Atomo('x')), Numero(-4.0))  // f(x) = x^2 + 4
    val root = newtonSolver.newton(expr, 'x', 0.0, maxIter = 10)
    root shouldBe None
  }

  it should "handle a zero derivative gracefully" in {
    val expr = Numero(1.0)  // f(x) = 1
    val root = newtonSolver.newton(expr, 'x', 0.0)
    root shouldBe None  // The derivative is zero, should return None
  }
}


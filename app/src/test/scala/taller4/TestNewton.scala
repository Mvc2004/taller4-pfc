package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestNewton extends AnyFunSuite {
  val newtonSolu = new Newton()


    test ("encontrar la raíz de una función lineal simple")  {
      val expr = Resta(Atomo('x'), Numero(5.0))
      val root = newtonSolu.newton(expr, 'x', 0.0)
      assert(root.contains(5.0), "Raíz esperada para la función lineal no encontrada")
    }

    test ("encontrar la raíz de una función cuadrática")  {
      val expr = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))  // f(x) = x^2 - 4
      val root = newtonSolu.newton(expr, 'x', 3.0)
      assert(math.abs(root.getOrElse(0.0) - 2.0) < 1e-6 , "Raíz esperada para la función cuadrática no encontrada")
    }

  test ("encontrar la raíz de una función más compleja")  {
      val expr = Suma(
        Prod(Numero(5.0), Atomo('k')),
        Div(Logaritmo(Numero(3.0)), Expo(Resta(Numero(8.0), Atomo('x')), Atomo('x')))
      )
      val root = newtonSolu.newton(expr, 'x', 1.0)
      assert(root.isDefined, "Raíz esperada para la función compleja no encontrada")
    }

    test ("devolver None si no se encuentra raíz dentro del número máximo de iteraciones")  {
      val expr = Resta(Prod(Atomo('x'), Atomo('x')), Numero(-4.0))  // f(x) = x^2 + 4
      val root = newtonSolu.newton(expr, 'x', 0.0, maxIter = 10)
      assert(root.isEmpty, "Se encontró una raíz cuando no se esperaba debido al máximo de iteraciones")
    }

   test ("manejar una derivada cero correctamente")  {
      val expr = Numero(1.0)  // f(x) = 1
      val root = newtonSolu.newton(expr, 'x', 0.0)
      assert(root.isEmpty, "Se encontró una raíz cuando no se esperaba debido a la derivada cero")
    }
  }

/**
  * Taller 3 - Programación Funcional
  * Autores: < Merly Velasquez Cortez - 2266016
 *             Sofia Castillo Giraldo - 2266149 >
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object Taller4{

  def saludo() = "Taller 4"

  def main(args: Array[String]): Unit = {
    println(saludo())
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
      }
    )
  }
 }

package testaus

import siirrot._
import scala.util.Random

/* Tulostettiin sarja satunnaisia laskuja. Ensimmäisessä testissä tarkastettiin oliko suunta vaihteen mukainen.
 * Arvioitiin myös suunnan järkevyys suhteessa asteisiin. Toisessa testissä tulostettiin kaikkilla viidellä
 * vaihdevaihtoehdolla, mutta tarkistettiin vain ensimmäinen kokonaan. Koska ensimmäinen funktio käyttää toista
 * funktiota apunaan ja se näyttää toimivan, on luultavaa, että myös apufunktio toimii oikein.
 */

object Suunta_apply_apply_suunnatVaihteella extends App {
  
  val random = new Random()
  
  println("Ensimmäinen apply")
  val asteet = Array.tabulate(10)(_ => random.nextDouble() * 2*Math.PI)
  var vaihteet = Array.tabulate(10)(_ => random.nextInt(5)+1)
  for (i <- 0 until 10) {
    println((asteet(i)*360)/(2*Math.PI)+", "+vaihteet(i)+" = "+Suunta(vaihteet(i), asteet(i)) )
  }
  
  println("")
  println("suunnatVaihteella")
  vaihteet = Array(1,2,3,4,5)
  for (vaihde <- vaihteet) {
    println("Vaihde: "+vaihde+"\n"+Suunta.suunnatVaihteella(vaihde).mkString(", ") )
  }
  
}
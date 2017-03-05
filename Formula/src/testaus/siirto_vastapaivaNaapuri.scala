package testaus

import siirrot._
import scala.util.Random

/* Tulostettiin sarja satunnaisia laskuja. Toistettiin kunnes kaikki päätapaustyypit oli kohdattu.
 * Tarkastettiin tulokset itse.
 */

object siirto_vastapaivaNaapuri extends App {
  val random = new Random()
  val siirrot = Array.tabulate(10)(_ => new Siirto(Koordinaatti(0,0),
                                                  Koordinaatti(random.nextInt(11)-5, random.nextInt(11)-5)))
  for (siirto <- siirrot) {
    println(siirto+" on vastapäivään käännettynä: "+siirto.vastapaivaNaapuri)
  }
  
}
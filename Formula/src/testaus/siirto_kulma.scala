package testaus

import siirrot._
import scala.util.Random

/* Tulostettiin sarja satunnaisia laskuja. Tutkittiin olivatko tulokset järkeviä.
 * Varmistettiin, että kulmat osuivat oikeaan neljännekseen ja että saman neljänneksen kulmat
 * olivat oikeassa järjestyksessä.
 */

object siirto_kulma extends App {
  val random = new Random()
  val siirrot = Array.tabulate(10)(_ => new Siirto(Koordinaatti(0,0),
                                                  Koordinaatti(random.nextInt(11)-5, random.nextInt(11)-5)))
  for (siirto <- siirrot) {
    println(siirto+"; kulma: "+(siirto.kulma/(2*Math.PI))*360)
  }
  
}
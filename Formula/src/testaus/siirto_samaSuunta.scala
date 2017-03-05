package testaus

import siirrot._
import scala.util.Random


/* Tulostettiin sarja satunnaisia laskuja. Tutkittiin tulosteita ja havaittiin, että näyttää toimivan niin kuin pitäisi.
 */

object siirto_samaSuunta extends App {
  
  val random = new Random()
  val siirrot = Array.tabulate(10)(_ => new Siirto(Koordinaatti(0,0),
                                                  Koordinaatti(random.nextInt(11)-5, random.nextInt(11)-5)))
  var vaihteet = Array.tabulate(10)(_ => random.nextInt(5)+1)
  for (i <- 0 until 10) {
    println("Siirto: "+siirrot(i)+", vaihde "+vaihteet(i)+" => "+siirrot(i).samaSuunta(vaihteet(i)))
  }
  
}
package testaus

import org.junit.Test
import org.junit.Assert._
import siirrot._
import scala.util.Random


/* Tulostettiin sarja satunnaisia laskuja. Tutkittiin tulosteita ja havaittiin, että näyttää toimivan niin kuin pitäisi.
 */

class siirto_samaSuunta{
  
  
  /* Aiempi testi
  val random = new Random()
  val siirrot = Array.tabulate(10)(_ => new Siirto(Koordinaatti(0,0),
                                                  Koordinaatti(random.nextInt(11)-5, random.nextInt(11)-5)))
  var vaihteet = Array.tabulate(10)(_ => random.nextInt(5)+1)
  for (i <- 0 until 10) {
    println("Siirto: "+siirrot(i)+", vaihde "+vaihteet(i)+" => "+siirrot(i).samaSuunta(vaihteet(i)))
  }*/
  
  //Uusi yksittäinen testi
  @Test def siirto = {
    val siirto = new Siirto(Koordinaatti(2,7), Koordinaatti(0,9))
    val siirtoVaihteella1 = siirto.samaSuunta(1)
    assert(siirtoVaihteella1 == Suunta(-1,1), "'Oikea' tulos on "+siirtoVaihteella1)
  }
  
  
  
}
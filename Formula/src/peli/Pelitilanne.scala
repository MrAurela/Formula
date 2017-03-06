
package peli

import pelikomponentit.Pelilauta
import siirrot.{Koordinaatti, Siirto}
import  pelikomponentit._

class Pelitilanne(lauta: Pelilauta, pelaajaLista: Vector[Pelaaja]) {
  
  val pelaajat = pelaajaLista
  val pelilauta = lauta
  
  var vuorossa = pelaajaLista(0)
 
  def eiVuorossa = pelaajaLista.find(_ != vuorossa)
  
  def siirraAutoa(kohde: Koordinaatti) {
    val siirtoOnnistui = this.pelilauta.siirraAutoa(vuorossa.auto, kohde)
    if (siirtoOnnistui) vaihdaVuoroa()
  }

  
  private def vaihdaVuoroa() = {
    if (pelaajat(0) == vuorossa) vuorossa = pelaajat(1)
    else vuorossa = pelaajat(0)
    vuorossa.auto.aloitaVuoro()
  }
  
}

object Pelitilanne {
  
  def apply() = new Pelitilanne(new Pelilauta(Vector(Vector())),
                              Vector(new Pelaaja(new Auto()), new Pelaaja(new Auto())))
      
}

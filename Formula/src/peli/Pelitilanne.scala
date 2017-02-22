
package peli

import pelikomponentit.Pelilauta
import siirrot.Koordinaatti

class Pelitilanne(lauta: Pelilauta, pelaajaLista: Vector[Pelaaja]) {
  
  val pelaajat = pelaajaLista
  val pelilauta = lauta
  
  var vuorossa = pelaajaLista(0)
  
  def siirraAutoa(kohde: Koordinaatti) {
    val siirtoOnnistui = this.pelilauta.siirraAutoa(vuorossa.auto, kohde)
    if (siirtoOnnistui) vaihdaVuoroa()
  }
  
  private def vaihdaVuoroa() = {
    if (pelaajat(0) == vuorossa) vuorossa = pelaajat(1)
    else vuorossa = pelaajat(0)
  }
  
}
package peli

import siirrot._

class AI(pelitilanne: Pelitilanne) {
  
  def siirto = {
    val vuorossa = pelitilanne.vuorossa
    var kuviteltuTilanne = new Pelitilanne(pelitilanne.pelilauta, pelitilanne.pelaajat)
    kuviteltuTilanne.siirraAutoa(Koordinaatti(1,1))
  }
  
}
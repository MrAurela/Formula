package peli

import siirrot._

class AI(pelitilanne: Pelitilanne) {
  
  def siirto: Koordinaatti = {
    val itse = pelitilanne.vuorossa
    val sijainti = pelitilanne.pelilauta.etsiAuto(itse.auto)
    var kuviteltuTilanne = new Pelitilanne(pelitilanne.pelilauta, pelitilanne.pelaajat)
    
    itse.auto.sallitutSuunnat(1).muutaSiirroksi(sijainti).kohdeKoordinaatti
  }
  
}
package peli

import siirrot._

class AI(pelitilanne_ : Pelitilanne) {
  
  var pelitilanne = pelitilanne_
  
  //TUTKI TOIMIIKO KUVITELTU TILANNE SITTEN JATKA TÄTÄ
  def siirto: Koordinaatti = {
    val itse = pelitilanne.vuorossa
    val sijainti = pelitilanne.pelilauta.etsiAuto(itse.auto)
    var kuviteltuTilanne = new Pelitilanne(pelitilanne.pelilauta, pelitilanne.pelaajat)
    
    println(kuviteltuTilanne.toString)
    
    
    itse.auto.sallitutSuunnat(1).muutaSiirroksi(sijainti).kohdeKoordinaatti
  }
  
}
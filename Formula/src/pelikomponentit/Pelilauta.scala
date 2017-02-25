package pelikomponentit

import siirrot.Koordinaatti

class Pelilauta(maastot: Vector[Vector[Maasto]]) {
  require(maastot.forall(_.length == maastot(0).length)) //Taulukon pitää olla suorakaide.
  
  val ruudut = maastot.map(_.map(new Ruutu(_))) //Vaihdetaan maastot vastaavaan ruutuun
  
  val korkeus = ruudut.length
  val leveys = ruudut(0).length
  
  def alustaAutot(autot: Vector[Auto]) = {
    for (pystySuorat <- ruudut; ruutu <- pystySuorat) {
      if (ruutu.maasto == AloitusRuutu1) {
        ruutu.lisaaAuto(autot(0))
      } else if (ruutu.maasto == AloitusRuutu2) {
        ruutu.lisaaAuto(autot(1))
      }
    }
  }
  
  def siirraAutoa(auto: Auto, kohde: Koordinaatti): Boolean = {
    if (kohde.onLaudalla(this)) {
      println(true)
      val lahto = this.etsiAuto(auto)
      ruudut(kohde.y)(kohde.x).lisaaAuto(auto)
      ruudut(lahto.y)(lahto.x).poistaAuto()
      true //Toistaiseksi tarkistetaan vain että siirto on laudalla
    } else {
      println(false)
      false
    }
  }
  
  private def etsiAuto(auto: Auto): Koordinaatti = {
    for (x <- 0 until leveys; y <- 0 until korkeus) {
      if (ruudut(y)(x).auto.getOrElse(new Auto()) == auto) 
        return Koordinaatti(x,y)
    }
    throw new Exception("Funktio etsiAuto, ei löytänyt autoa.")
  }
  
}
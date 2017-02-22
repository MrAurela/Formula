package pelikomponentit

import siirrot.Koordinaatti

class Pelilauta(maastot: Vector[Vector[Maasto]]) {
    
  val ruudut = maastot.map(_.map(new Ruutu(_))) //Vaihdetaan maastot vastaavaan ruutuun
  
  val korkeus = ruudut(0).length
  val leveys = ruudut.flatten.length / korkeus //Suorakaiteen mallisen radan leveys saadaan jakamalla
                                               //koko pinta-ala korkeudella.
  
  def alustaAutot() = {
    for (pystySuorat <- ruudut; ruutu <- pystySuorat) {
      if (ruutu.maasto == AloitusRuutu1) {
        ruutu.lisaaAuto(new Auto)
      } else if (ruutu.maasto == AloitusRuutu2) {
        ruutu.lisaaAuto(new Auto)
      }
    }
  }
  
  def siirraAutoa(auto: Auto, kohde: Koordinaatti): Boolean = {
    val lahto = this.etsiAuto(auto)
    ruudut(kohde.y)(kohde.x).lisaaAuto(auto)
    ruudut(lahto.y)(lahto.x).poistaAuto()
    true //Toistaiseksi ei tarkasteta onko siirto laillinen
  }
  
  private def etsiAuto(auto: Auto): Koordinaatti = {
    var koordinaatti = Koordinaatti(-1,-1)
    for (x <- 0 until leveys; y <- 0 until korkeus) {
      if (ruudut(y)(x).auto.getOrElse(new Auto()) == auto) koordinaatti = Koordinaatti(y,x)
    }
    koordinaatti
  }
  
}
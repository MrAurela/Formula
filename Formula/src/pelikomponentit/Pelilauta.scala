package pelikomponentit

import siirrot.{Koordinaatti, Siirto, Suunta}

class Pelilauta(maastot: Vector[Vector[Maasto]]) {
  require(maastot.forall(_.length == maastot(0).length)) //Taulukon pitää olla suorakaide.
  
  val ruudut = maastot.map(_.map(new Ruutu(_))) //Vaihdetaan maastot vastaavaan ruutuun
  
  val korkeus = ruudut.length
  val leveys = ruudut(0).length
  
  def apply(koordinaatti: Koordinaatti) = {
    ruudut(koordinaatti.y)(koordinaatti.x)
  }
  
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
    if (this.sallitutKoordinaatit(auto).contains(kohde)) { //Jos siirto kuuluu laillisiin siirtoihin.
      val lahto = this.etsiAuto(auto)
      ruudut(kohde.y)(kohde.x).lisaaAuto(auto) //Lisätään auto uuteen ruutun
      ruudut(lahto.y)(lahto.x).poistaAuto() //Poistetaan vanhasta
      auto.merkitseSiirto(lahto, kohde) //Merkitään siirto auton muistiin.
      true
    } else {
      false
    }
  }
  
  //Ottaa huomioon vaihteen ja suunnan
  def mahdollisetSuunnat(auto: Auto): Vector[Suunta] = {
    auto.sallitutSuunnat
  }
  
  //Ottaa vaihteen ja suunnan lisäksi huomioon toisen auton sijainnin ja laudan reunat
  def sallitutSiirrot(auto: Auto): Vector[Siirto] = {
    val lahto = this.etsiAuto(auto)
    val suunnat = this.mahdollisetSuunnat(auto).filter{ suunta: Suunta =>
      val kohde = suunta.muutaSiirroksi(lahto).kohdeKoordinaatti
      kohde.onLaudalla(this) && this(kohde).eiAutoa && this(kohde).voiAjaa
    }
    suunnat.map(_.muutaSiirroksi(lahto))
  }
  
  //Koordinaatit, joihin on mahdollista siirtyä.
  def sallitutKoordinaatit(auto: Auto) = this.sallitutSiirrot(auto).map(_.kohdeKoordinaatti)
  
  private def etsiAuto(auto: Auto): Koordinaatti = {
    for (x <- 0 until leveys; y <- 0 until korkeus) {
      if (ruudut(y)(x).auto.getOrElse(new Auto()) == auto) 
        return Koordinaatti(x,y)
    }
    throw new Exception("Funktio etsiAuto, ei löytänyt autoa.")
  }
  
}
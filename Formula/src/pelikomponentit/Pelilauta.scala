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
  
  def siirraAutoaLaillisesti(auto: Auto, kohde: Koordinaatti): Boolean = {
    if (this.sallitutKoordinaatit(auto).contains(kohde)) { //Jos siirto kuuluu laillisiin siirtoihin.
      this.siirraAutoaPakolla(auto, kohde, true)
      true
    } else {
      false
    }
  }
  
  def siirraAutoaPakolla(auto: Auto, kohde: Koordinaatti, merkitseSiirto: Boolean) = {
    val lahto = this.etsiAuto(auto)
    ruudut(kohde.y)(kohde.x).lisaaAuto(auto) //Lisätään auto uuteen ruutun
    ruudut(lahto.y)(lahto.x).poistaAuto() //Poistetaan vanhasta
    if (merkitseSiirto) auto.merkitseSiirto(lahto, kohde) //Merkitään siirto auton muistiin.
    auto.lisaaKierros(
        this.siirtoMaaliin( auto.edellinenSiirto.getOrElse( new Siirto(Koordinaatti(0,0), Koordinaatti(0,0)) ) )
    )
  }
  /*
  def siirraAutoaSuuntaan(auto: Auto, suunta: Suunta): Boolean = {
    val kohde = ( suunta.muutaSiirroksi(this.etsiAuto(auto)) ).kohdeKoordinaatti
    siirraAutoa(auto, kohde)
  }*/
  
  //Ottaa huomioon vaihteen ja suunnan
  def mahdollisetSuunnat(auto: Auto): Vector[Suunta] = {
    auto.sallitutSuunnat
  }
  
  //Ottaa vaihteen ja suunnan lisäksi huomioon toisen auton sijainnin ja laudan reunat
  def sallitutSiirrot(auto: Auto): Vector[Siirto] = {
    val lahto = this.etsiAuto(auto)
    val suunnat = this.mahdollisetSuunnat(auto)
    val siirrot = suunnat.map(_.muutaSiirroksi(lahto))
    siirrot.filter{ siirto: Siirto =>
      val kohde = siirto.kohdeKoordinaatti
      kohde.onLaudalla(this) && this.lapimentavatRuudut(siirto).forall(_.voiAjaa)
    }
  }
  
  //Koordinaatit, joihin on mahdollista siirtyä.
  def sallitutKoordinaatit(auto: Auto) = {
    this.sallitutSiirrot(auto).map(_.kohdeKoordinaatti)
  }
  
  //Palauttaa listan ruuduista, joiden läpi siirrytään.
  def lapimentavatRuudut(siirto: Siirto): Vector[Ruutu] = {
    val koordinaatit = siirto.lapimentavatKoordinaatit
    koordinaatit.map( this(_) )
  }
  
  //Palauttaa -1, 0 tai 1 riippuen, onko maaliruutua saavutettu tai ohitettu siirrolla.
  //1 kuvaa maalin läpimenoa oikeaan kiertosuuntaan (vasemmalla) ja -1 taas maalin läpäisyä väärään suuntaan (oikealle)
  //0 on siirto, joka ei kulje maaliruutujen läpi tai liikkuu maaliruuduissa 
  def siirtoMaaliin(siirto: Siirto) = {
    if (lapimentavatRuudut(siirto).exists(_.onMaali) && siirto.xLiike > 0) -1
    else if (lapimentavatRuudut(siirto).exists(_.onMaali) && siirto.xLiike < 0) 1
    else 0
  }
  
  private def etsiAuto(auto: Auto): Koordinaatti = {
    for (x <- 0 until leveys; y <- 0 until korkeus) {
      if (ruudut(y)(x).auto.getOrElse(new Auto) == auto) 
        return Koordinaatti(x,y)
    }
    throw new Exception("Funktio etsiAuto, ei löytänyt autoa.")
  }
  
}
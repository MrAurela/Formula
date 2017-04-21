package pelikomponentit

import siirrot.{Koordinaatti, Siirto, Suunta}
import peli.{Peli, Pelaaja, AI}
import tietojenTallennus.{Rata, Profiili}
import pelikomponentit._

import scala.util.Random


class Pelilauta(radanTiedot: Rata) {
  
  val rata = radanTiedot
  val nimi = rata.nimi
  val maastot = rata.muoto
  
  val ruudut = maastot.map(_.map(new Ruutu(_))) //Vaihdetaan maastot vastaavaan ruutuun
  
  val korkeus = ruudut.length
  val leveys = ruudut(0).length
  
  def apply(koordinaatti: Koordinaatti) = {
    ruudut(koordinaatti.y)(koordinaatti.x)
  }
  
  /* Alkuperäinen autojen alustus tapahtui valitut ruudun perusteella. Vaihdettiin tehtävänannon mukaiseen satunnaisuuteen.
  def alustaAutot(autot: Vector[Auto]) = {
    for (pystySuorat <- ruudut; ruutu <- pystySuorat) {
      if (ruutu.maasto == AloitusRuutu1) {
        ruutu.lisaaAuto(autot(0))
      } else if (ruutu.maasto == AloitusRuutu2) {
        ruutu.lisaaAuto(autot(1))
      }
    }
  }*/
  def alustaAutot(autot: Vector[Auto]) = {
    val vaihtoehdot = ruudut.flatten.filter(_.onMaali).toBuffer
    require(vaihtoehdot.size >= autot.size)
    val r = new Random
    
    autot.foreach { auto =>
      val valinta = r.nextInt(vaihtoehdot.size)
      vaihtoehdot(valinta).lisaaAuto(auto) //Lisätäään auto satunnaiseen ruutuun
      auto.asetaAloitusSuunta(vaihtoehdot(valinta).maasto.tyyppi)
      vaihtoehdot.remove(valinta) //Poistetaan ruutu valikoimasta
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
        this.siirtoMaaliin( new Siirto(lahto, kohde) )
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
  def kaikkiMahdollisetSuunnat(auto: Auto): Vector[Suunta] = {
    auto.kaikkiSallitutSuunnat
  }
  
  def vaihteenSiirrot(auto: Auto): Vector[Siirto] = {
    val lahto = this.etsiAuto(auto)
    this.mahdollisetSuunnat(auto).map(_.muutaSiirroksi(lahto))
  }
  
  //Ottaa vaihteen ja suunnan lisäksi huomioon toisen auton sijainnin ja laudan reunat.
  //vainVaihteella arvo ilmaisee, palautetaanko kaikki auton siirtovaihtoehdot (sisältää vaihteenvaihdon), vai vaan nykyisen vaihteen siirrot.
  def sallitutSiirrot(auto: Auto, vainVaihteella: Boolean): Vector[Siirto] = {
    val lahto = this.etsiAuto(auto)
    val suunnat = if (vainVaihteella) this.mahdollisetSuunnat(auto)
                  else this.kaikkiMahdollisetSuunnat(auto)
    val siirrot = suunnat.map(_.muutaSiirroksi(lahto))
    siirrot.filter(_.kohdeKoordinaatti.onLaudalla(this)).filter{siirto =>
      val lapimentavatRuudut = this.lapimentavatRuudut(siirto)
      lapimentavatRuudut.forall(_.voiAjaa) && //Yleinen laillisuus
      Maasto.hiekanSaannot(this, lapimentavatRuudut, siirto, auto) && //Maastojen säännöt
      Maasto.syvanHiekanSaannot(this, lapimentavatRuudut, siirto, auto) &&
      Maasto.jaanSaannot(this, lapimentavatRuudut, siirto, auto) &&
      Maasto.oljynSaannot(this, lapimentavatRuudut, siirto, auto)
    }
  }
  
  //Koordinaatit, joihin on mahdollista siirtyä.
  def sallitutKoordinaatit(auto: Auto) = {
    this.sallitutSiirrot(auto, true).map(_.kohdeKoordinaatti)
  }
  
  //Palauttaa listan ruuduista, joiden läpi siirrytään.
  def lapimentavatRuudut(siirto: Siirto): Vector[Ruutu] = {
    val koordinaatit = siirto.lapimentavatKoordinaatit
    koordinaatit.map( this(_) )
  }
  
  //Palauttaa -1, 0 tai 1 riippuen, onko maaliruutua saavutettu tai ohitettu siirrolla.
  //1 kuvaa maalin läpimenoa oikeaan kiertosuuntaan (vasemmalla) ja -1 taas maalin läpäisyä väärään suuntaan (oikealle)
  //0 on siirto, joka ei kulje maaliruutujen läpi tai liikkuu maaliruuduissa 
  def siirtoMaaliin(siirto: Siirto): Int = {
    val maaliruudut = lapimentavatRuudut(siirto).filter(_.onMaali)
    var pisteet = 0
    maaliruudut.foreach{ruutu =>
      ruutu.maasto match {
        case MaaliYlos => 
          if (siirto.yLiike < 0) pisteet +=1 
          else pisteet -= 1
        case MaaliAlas => 
          if (siirto.yLiike > 0) pisteet +=1 
          else pisteet -= 1
        case MaaliOikea =>
          if (siirto.xLiike > 0) pisteet +=1 
          else pisteet -= 1
        case MaaliVasen =>
          if (siirto.xLiike < 0) pisteet +=1 
          else pisteet -= 1
        case _ => pisteet = pisteet
      }
    }
    //Vaikka kuljettaisiin "monen maaliruudun" läpi yhdellä siirrolla esimerkiksi vinottaisella siirrolla, lasketaan tämä vain yhdeksi
    Math.signum(pisteet).toInt
  }
  
  def etsiAuto(auto: Auto): Koordinaatti = {
    for (x <- 0 until leveys; y <- 0 until korkeus) {
      if (ruudut(y)(x).auto.getOrElse(new Auto) == auto) 
        return Koordinaatti(x,y)
    }
    throw new Exception("Funktio etsiAuto, ei löytänyt autoa.")
  }
  
}


object Pelilauta {
 

  
}
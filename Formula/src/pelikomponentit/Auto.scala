package pelikomponentit

import scala.collection.mutable.Buffer
import siirrot.{Siirto, Suunta, Koordinaatti}
import peli.{Peli, Pelitilanne}

class Auto() {

  var vaihde = 1
  var vaihdeVuoronAlussa = 1
  var kierrokset = 0
  var aloitusSuunta = Suunta(Koordinaatti(0,-1)) //Oletussuunta. Oikea suunta asetetaan kun auto alustetaan Pelilauta.alustaAutot -funktiossa.
  
  private val siirtoLista = Buffer[Siirto]()
  def siirrot = this.siirtoLista.toVector
  
  def aloitaVuoro() = {
    vaihdeVuoronAlussa = vaihde
  }
  
  def sallitutSuunnat: Vector[Suunta] = {
    if (tehdytSiirrot > 0) {
      val viimeSiirto = siirrot.last
      val samaanSuuntaan = viimeSiirto.samaSuunta(vaihde)
      val myotapaivainenSuunta = samaanSuuntaan.myotapaivaNaapuri.muutaSuunnaksi
      val vastapaivainenSuunta = samaanSuuntaan.vastapaivaNaapuri.muutaSuunnaksi
      Vector(vastapaivainenSuunta, samaanSuuntaan, myotapaivainenSuunta)
    } else {
      val lahtoSuunta = aloitusSuunta
      val myotapaivainenSuunta = lahtoSuunta.myotapaivaNaapuri.muutaSuunnaksi
      val vastapaivainenSuunta = lahtoSuunta.vastapaivaNaapuri.muutaSuunnaksi
      Vector(vastapaivainenSuunta, lahtoSuunta, myotapaivainenSuunta)
    }
  }
  
  def kaikkiSallitutSuunnat: Vector[Suunta] = {
    val suoraan = this.sallitutSuunnat(1)
    val kaikkiSuunnat = this.sallitutSuunnat.toBuffer
    if (vaihdettaVoiNostaa) {
      val isompiSuoraan = suoraan.samaSuunta(this.vaihde+1)
      kaikkiSuunnat.append(isompiSuoraan.vastapaivaNaapuri.muutaSuunnaksi, isompiSuoraan, isompiSuoraan.myotapaivaNaapuri.muutaSuunnaksi)
    }
    if (vaihdettaVoiLaskea) {
      val pienempiSuoraan = suoraan.samaSuunta(this.vaihde-1)
      kaikkiSuunnat.append(pienempiSuoraan.vastapaivaNaapuri.muutaSuunnaksi, pienempiSuoraan, pienempiSuoraan.myotapaivaNaapuri.muutaSuunnaksi)
    }
    kaikkiSuunnat.toVector
  }
  
  def eiVoiLiikkua(pelitilanne: Pelitilanne): Boolean = pelitilanne.kaikkiSallitutSiirrot.isEmpty
  
  //Vaihdetta ei voi vaihtaa ensimmäisellä kierroksella
  def vaihdettaVoiNostaa = vaihde <= vaihdeVuoronAlussa && vaihde < 5 && tehdytSiirrot > 0
  
  def vaihdettaVoiLaskea = vaihde >= vaihdeVuoronAlussa && vaihde > 1 && tehdytSiirrot > 0
  
  def nostaVaihdetta() = {
    if (vaihdettaVoiNostaa) vaihde += 1
  }
  
  def laskeVaihdetta() = {
    if (vaihdettaVoiLaskea) vaihde -= 1
  }
  
  def merkitseSiirto(lahto: Koordinaatti, kohde: Koordinaatti) {
    this.siirtoLista.append(new Siirto(lahto, kohde))
  }
  
  def asetaAloitusSuunta(ruutu: Maasto) = ruutu match {
    case MaaliYlos => this.aloitusSuunta = Suunta(Koordinaatti(0,-1))
    case MaaliAlas => this.aloitusSuunta = Suunta(Koordinaatti(0,1))
    case MaaliOikea => this.aloitusSuunta = Suunta(Koordinaatti(1,0))
    case _ => this.aloitusSuunta = Suunta(Koordinaatti(-1,0)) //Oletusvaihtoehto
  }
  
  //Lisää, vähentää tai pitää kierroksen samana syötteen (1,0,-1) perusteella.
  def lisaaKierros(lisays: Int) = this.kierrokset += Math.signum(lisays).toInt
  
  def poistaEdellinenSiirto() = if (this.liikkunut) this.siirtoLista.remove(tehdytSiirrot-1)
  def palautaEdellinenVaihde() = if (this.liikkunut) vaihde = this.siirrot(this.tehdytSiirrot-1).vaihde //Vaihde on edellisen siirron vaihde
  
  def tehdytSiirrot = this.siirrot.length
  def edellinenSiirto = if (this.liikkunut) Some(this.siirrot(tehdytSiirrot-1)) else None
  
  def liikkunut = this.tehdytSiirrot > 0
  

  
}
package pelikomponentit

import scala.collection.mutable.Buffer
import siirrot.{Siirto, Suunta, Koordinaatti}

class Auto {
  
  var vaihde = 1
  var vaihdeVuoronAlussa = 1
  
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
      val lahtoSuunta = Suunta(1, Math.PI) //Alkuperäinen suunta, suoraan vasemmalle
      val myotapaivainenSuunta = lahtoSuunta.myotapaivaNaapuri.muutaSuunnaksi
      val vastapaivainenSuunta = lahtoSuunta.vastapaivaNaapuri.muutaSuunnaksi
      Vector(vastapaivainenSuunta, lahtoSuunta, myotapaivainenSuunta)
    }
  }
  
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
  
  def poistaEdellinenSiirto() = if (this.liikkunut) this.siirtoLista.remove(tehdytSiirrot-1)
  def palautaEdellinenVaihde() = if (this.liikkunut) vaihde = this.siirrot(this.tehdytSiirrot-1).vaihde //Vaihde on edellisen siirron vaihde
  
  def tehdytSiirrot = this.siirrot.length
  def edellinenSiirto = if (this.liikkunut) Some(this.siirrot(tehdytSiirrot-1)) else None
  
  def liikkunut = this.tehdytSiirrot > 0
  

  
}
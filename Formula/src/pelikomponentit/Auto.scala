package pelikomponentit

import scala.collection.mutable.Buffer
import siirrot.{Siirto, Suunta, Koordinaatti}

class Auto {
  
  var vaihde = 1
  var vaihdeVuoronAlussa = 1
  
  val siirrot = Buffer[Siirto]()
  
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
      val lahtoSuunta = Suunta(1, Math.PI)
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
    siirrot.append(new Siirto(lahto, kohde))
  }
  
  def tehdytSiirrot = this.siirrot.length
  

  
}
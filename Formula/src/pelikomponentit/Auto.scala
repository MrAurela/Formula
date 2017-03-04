package pelikomponentit

import scala.collection.mutable.Buffer
import siirrot.{Siirto, Koordinaatti}

class Auto {
  
  var vaihde = 1
  var vaihdeVuoronAlussa = 1
  
  val siirrot = Buffer[Siirto]()

  //var kuljettuReitti = 
  
  def aloitaVuoro() = {
    vaihdeVuoronAlussa = vaihde
  }
  
  def vaihdettaVoiNostaa = vaihde <= vaihdeVuoronAlussa && vaihde < 5
  
  def vaihdettaVoiLaskea = vaihde >= vaihdeVuoronAlussa && vaihde > 1
  
  def nostaVaihdetta() = {
    if (vaihdettaVoiNostaa) vaihde += 1
  }
  
  def laskeVaihdetta() = {
    if (vaihdettaVoiLaskea) vaihde -= 1
  }
  
  def merkitseSiirto(lahto: Koordinaatti, kohde: Koordinaatti) {
    siirrot.append(new Siirto(lahto, kohde))
  }
  

  
}
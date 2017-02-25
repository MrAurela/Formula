package pelikomponentit

class Auto {
  
  var vaihde = 1
  var vaihdeVuoronAlussa = 1

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
  

  
}
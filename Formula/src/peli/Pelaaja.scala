package peli

import pelikomponentit.Auto

class Pelaaja(ai: Boolean) {
  
  val auto = new Auto()
  var tekoaly: Option[AI] = None
  
  def onPelaaja = tekoaly.isDefined
  
}
package peli

import pelikomponentit.Auto

class Pelaaja(auto_ : Auto) {
  
  val auto = auto_
  var tekoaly: Option[AI] = None
  
  def onPelaaja = tekoaly.isDefined
  
  def asetaAI(ai: AI) = tekoaly = Some(ai)
  
}
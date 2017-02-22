package peli

import pelikomponentit.Auto

class Pelaaja() {
  
  val auto = new Auto()
  var tekoaly: Option[AI] = None
  
  def onPelaaja = tekoaly.isDefined
  
  def asetaAI(ai: AI) = tekoaly = Some(ai)
  
}
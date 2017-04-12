package peli

import pelikomponentit.Auto
import tietojenTallennus.Profiili

class Pelaaja(profiili_ : Option[Profiili], auto_ : Auto) {
  
  val auto = auto_
  var profiili: Option[Profiili] = profiili_
  var tekoaly: Option[AI] = None

  
  def onPelaaja = !tekoaly.isDefined
  
  def asetaAI(ai: AI) = tekoaly = Some(ai)
  
}

object Pelaaja {
  def apply(profiili: Option[Profiili]) = new Pelaaja(profiili, new Auto)
}
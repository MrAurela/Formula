package peli

import pelikomponentit.Auto
import tietojenTallennus.Profiili

class Pelaaja(profiili_ : Option[Profiili], auto_ : Auto) {
  
  override def toString(): String = {
    if (profiili.isDefined) profiili.get.nimi
    else if (Peli.pelitilanne.isDefined) {
      if (Peli.pelitilanne.get.pelaajat(0) == this) "Sininen"
      else if (Peli.pelitilanne.get.pelaajat(1) == this) "Punainen"
      else "EI PROFIILIA"
    } else "EI PROFIILIA"
  }
  
  val auto = auto_
  var profiili: Option[Profiili] = profiili_
  var tekoaly: Option[AI] = None

  
  def onPelaaja = !tekoaly.isDefined
  
  def asetaAI(ai: AI) = {
    tekoaly = Some(ai)
  }
  
  def onTekoaly = tekoaly.isDefined
  
}

object Pelaaja {
  def apply(profiili: Option[Profiili]) = new Pelaaja(profiili, new Auto)
}
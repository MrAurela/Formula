package gui

import peli.Peli

object VaihteenHallinta {
  
  val vaihdeYlos = Ikkuna.vaihteenVaihto.vaihdeYlos
  val vaihdeAlas = Ikkuna.vaihteenVaihto.vaihdeAlas
  val ruudukko = Ikkuna.ruudukko
  
  def nostaVaihdetta() = Peli.pelitilanne.get.vuorossa.auto.nostaVaihdetta()
  
  def laskeVaihdetta() = {
    Peli.pelitilanne.get.vuorossa.auto.laskeVaihdetta()
  }
  
  def paivita() = {
    paivitaNappulat()
    paivitaVaihde()
    ruudukko.repaint()
  }
  
  private def paivitaNappulat() {
    val auto = Peli.pelitilanne.get.vuorossa.auto
    
    if (auto.vaihdettaVoiNostaa) vaihdeYlos.enabled = true //Enabloidaan/diabldoiaan nappulat
    else vaihdeYlos.enabled = false
    if (auto.vaihdettaVoiLaskea) vaihdeAlas.enabled = true
    else vaihdeAlas.enabled = false
    
  }
  
  private def paivitaVaihde() {
    Ikkuna.vaihteenVaihto.vaihdeLuku.text = Peli.pelitilanne.get.vuorossa.auto.vaihde.toString
    
  }
  
}
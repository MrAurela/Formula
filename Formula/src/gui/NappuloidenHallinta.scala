package gui

import peli.Peli
import scala.swing._

object NappuloidenHallinta {
  
  val vaihdeYlos = Ikkuna.vaihteenVaihto.vaihdeYlos
  val vaihdeAlas = Ikkuna.vaihteenVaihto.vaihdeAlas
  val peruSiirto = Ikkuna.oikeaPuoli.peruSiirto
  
  def nostaVaihdetta() = Peli.pelitilanne.get.vuorossa.auto.nostaVaihdetta()
  
  def laskeVaihdetta() = Peli.pelitilanne.get.vuorossa.auto.laskeVaihdetta()
  
  def peruEdellinenSiirto() = if (Peli.pelitilanne.isDefined) Peli.pelitilanne.get.peruSiirto()
  
  def paivita() = {
    paivitaNappulat()
    paivitaVaihde()

  }
  
  private def paivitaNappulat() {
    val auto = Peli.pelitilanne.get.vuorossa.auto //Holtitonta gettausta
    val eiVuorossa = Peli.pelitilanne.get.eiVuorossa.auto
    
    if (auto.vaihdettaVoiNostaa) vaihdeYlos.enabled = true //Enabloidaan/diabldoiaan nappulat
    else vaihdeYlos.enabled = false
    if (auto.vaihdettaVoiLaskea) vaihdeAlas.enabled = true
    else vaihdeAlas.enabled = false
    
    if ( eiVuorossa.liikkunut) peruSiirto.enabled = true
    else peruSiirto.enabled = false
    
  }
  
  private def paivitaVaihde() {
    Ikkuna.vaihteenVaihto.vaihdeLuku.text = Peli.pelitilanne.get.vuorossa.auto.vaihde.toString
  }
  
}
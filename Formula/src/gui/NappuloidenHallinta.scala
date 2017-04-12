package gui

import peli.{Peli, Pelitilanne, Pelaaja}
import siirrot.Koordinaatti
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
  
  //Tekee siirron ja palauttaa tiedon siitä, jatkuuko peli edelleen.
  def teeSiirto(koordinaatti: Koordinaatti): Boolean = {
    val pelitilanne = Peli.pelitilanne
    if (pelitilanne.isDefined) { //Funktiota kutsutaan vain kun pelitilanne on määritelty. Varmuuden vuoksi tarkistetaan asia.
      pelitilanne.get.siirraAutoa(koordinaatti)
      pelitilanne.get.peliKaynnissa
    } else true
  }
  
  def uusiPeli(): Unit = {
    val valikko = Ikkuna.paaValikko
    val rata = valikko.tasovalinta.radat
                      .get(valikko.tasovalinta.menulista //Otetaan rata Option muodossa
                        .find(_.selected) //Valittu teksti
                        .getOrElse(new RadioMenuItem("")) //Jos mitään rataa ei ole valittu (ei tosin pitäisi olla mahdollista)
                      )
    val profiili1 = valikko.pelaaja1.profiilit
                      .get(valikko.pelaaja1.menulista
                        .find(_.selected)
                        .getOrElse(new RadioMenuItem(""))
                      )
    val profiili2 = valikko.pelaaja2.profiilit
                      .get(valikko.pelaaja2.menulista
                        .find(_.selected)
                        .getOrElse(new RadioMenuItem(""))
                      )
    if (rata.isDefined) { //Kunhan rata on määritelty. Profiili voi olla myös None, jos "EI PROFIILIA" vaihtoehto on valittu.
      val uusiPeli = Peli.uusiPeli(rata.get, Vector(profiili1, profiili2))
      Ikkuna.paaIkkuna.vaihdaIkkunanSisalto(Ikkuna.peliIkkuna(uusiPeli))
    }
  }
  
  
  private def paivitaNappulat() {
    if (Peli.pelitilanne.isDefined) { // Päivitys on tarpeellinen vain jos peli on käynnissä.
      val auto = Peli.pelitilanne.get.vuorossa.auto
      
      val eiVuorossa = Peli.pelitilanne.get.eiVuorossa.auto
    
      if (auto.vaihdettaVoiNostaa) vaihdeYlos.enabled = true //Enabloidaan/diabldoiaan nappulat
      else vaihdeYlos.enabled = false
      if (auto.vaihdettaVoiLaskea) vaihdeAlas.enabled = true
      else vaihdeAlas.enabled = false
      
      if ( eiVuorossa.liikkunut) peruSiirto.enabled = true
      else peruSiirto.enabled = false
    }
    
  }
  
  private def paivitaVaihde() {
    if (Peli.pelitilanne.isDefined) { //Päivitys on tarpeellinen vain, jos peli on käynnissä
      Ikkuna.vaihteenVaihto.vaihdeLuku.text = Peli.pelitilanne.get.vuorossa.auto.vaihde.toString
    }
  }
  
}
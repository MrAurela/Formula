package gui

import peli.{Peli, Pelitilanne, Pelaaja}
import siirrot.Koordinaatti
import tietojenTallennus.Profiili
import scala.swing._
import javax.swing.JOptionPane

//dialog input: http://stackoverflow.com/questions/19603403/how-to-collect-input-from-user-with-dialog-box

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
  def teeSiirto(koordinaatti: Koordinaatti) = {
    val pelitilanne = Peli.pelitilanne
    if (pelitilanne.isDefined) { //Funktiota kutsutaan vain kun pelitilanne on määritelty. Varmuuden vuoksi tarkistetaan asia.
      pelitilanne.get.siirraAutoa(koordinaatti)
    }
  }
  
  def uusiPeli(): Unit = {
    val valikko = this.paaValikko
    val rata = valikko.tasovalinta.valittu
    val profiili1 = valikko.pelaaja1.valittu
    val profiili2 = valikko.pelaaja2.valittu
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
  
  val paaValikko = new MenuBar {
    val pelaaja1 = new PelaajaMenu("Pelaaja1: Sininen")
    val pelaaja2 = new PelaajaMenu("Pelaaja2: Punainen")
    val tasovalinta = new RataMenu("Rata")
    contents += pelaaja1
    contents += pelaaja2
    contents += tasovalinta
  }

  
  def profiiliValikko: MenuBar= new MenuBar {
    contents += new Menu("Menu") {
      contents += new MenuItem(Action("Uusi profiili"){NappuloidenHallinta.luoUusiProfiili()})
      contents += new MenuItem(Action("Palaa päävalikkoon")(NappuloidenHallinta.palaaMenuun()))
    }
    def profiiliMenu = new Menu("Profiilien tiedot") {
      val menulista: Vector[MenuItem] = Peli.profiiliLista.map(profiili => 
        new MenuItem(Action(profiili.nimi)(NappuloidenHallinta.haeProfiilienTiedot(profiili)) ) )
      menulista.foreach(contents += _) //Lisätään kaikki profiilit menuun
    }
    contents += profiiliMenu
  }
  
  def haeProfiilienTiedot(profiili: Profiili) {
    Ikkuna.paaIkkuna.contents = Ikkuna.profiilienHallinta(profiili)
  }
  
  def luoUusiProfiili() {
    val nimi = JOptionPane.showInputDialog("Valitse profiilin nimi:")
    Peli.uusiProfiili(Profiili(nimi))
  }
  
  //Tämän funktion ainut tarkoitus on kiertää rekursiivinen viittaus, joka olisi syntynyt, jos 
  //profiilivalikossa olisi kutsuttu alla olevaa metodia.
  def palaaMenuun() {
    Ikkuna.paaIkkuna.vaihdaIkkunanSisaltoMenuun()
  }

  def tallenaProfiilienTiedot(pelitilanne: Pelitilanne) = {
    val voittaja = pelitilanne.tarkistaVoitto._1
    val selitys = pelitilanne.tarkistaVoitto._2
    if (voittaja.isDefined) {
      pelitilanne.pelaajat.foreach { pelaaja => 
        if (pelaaja == voittaja.get) pelaaja.profiili.foreach(_.paivita(true, pelitilanne.pelilauta.radanNimi, pelaaja.auto.tehdytSiirrot,selitys))
        else pelaaja.profiili.foreach(_.paivita(false, pelitilanne.pelilauta.radanNimi, pelaaja.auto.tehdytSiirrot, selitys))
      }
    }
  }
  
  
}
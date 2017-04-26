package gui

import peli.{Peli, Pelitilanne, Pelaaja,AI}
import siirrot.Koordinaatti
import tietojenTallennus.{Profiili, Rata}
import pelikomponentit.Maasto
import scala.swing._
import javax.swing.JOptionPane

//dialog input: http://stackoverflow.com/questions/19603403/how-to-collect-input-from-user-with-dialog-box

object NappuloidenHallinta {
  
  //Peli-ikkuna-----------------------------------------------------------------------------------------------------------
  val vaihdeYlos = Ikkuna.vaihteenVaihto.vaihdeYlos
  val vaihdeAlas = Ikkuna.vaihteenVaihto.vaihdeAlas
  val peruSiirto = Ikkuna.peruSiirto
  
  def nostaVaihdetta() = Peli.pelitilanne.get.vuorossa.auto.nostaVaihdetta()
  
  def laskeVaihdetta() = Peli.pelitilanne.get.vuorossa.auto.laskeVaihdetta()
  
  //Tekee siirron ja palauttaa tiedon siitä, jatkuuko peli edelleen.
  def teeSiirto(koordinaatti: Koordinaatti) = {
    val pelitilanne = Peli.pelitilanne
    if (pelitilanne.isDefined) { //Funktiota kutsutaan vain kun pelitilanne on määritelty. Varmuuden vuoksi tarkistetaan asia.
      pelitilanne.get.siirraAutoa(koordinaatti)
    }
  }
  
  def peruEdellinenSiirto() = {
    val tilanne = Peli.pelitilanne
    if (tilanne.isDefined) {
      tilanne.get.peruSiirto()
      if (tilanne.get.vuorossa.onTekoaly) { //Jos perumisen jälkeen tietokone olisi vuorossa, perutaan myös oma siirto
        tilanne.get.peruSiirto()
      }
    }
  }
  
  def lopetaPeli() {
    val vastaus = Dialog.showConfirmation(null, "Pelin tiedot menetetään, jos se keskeytetään. \n Oletko varma, että haluat lopettaa?",
                                          "Varoitus", Dialog.Options.YesNo, Dialog.Message.Warning)
    if (vastaus == Dialog.Result.Yes) Ikkuna.paaIkkuna.vaihdaIkkunanSisaltoMenuun()
  }
  
  def paivita() = {
    paivitaNappulat()
    paivitaVaihde()
    paivitaVuorossa()
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
  
  private def paivitaVuorossa() {
    if (Peli.pelitilanne.isDefined) { //Päivitys on tarpeellinen vain, jos peli on käynnissä
      Ikkuna.oikeaPuoli.vari.text = if (Peli.pelitilanne.get.vuorossa == Peli.pelitilanne.get.pelaajat(0)) "Sininen" else "Punainen"
      Ikkuna.oikeaPuoli.pelaaja.text = Peli.pelitilanne.get.vuorossa.toString()
      if (Ikkuna.oikeaPuoli.vari.text == Ikkuna.oikeaPuoli.pelaaja.text) Ikkuna.oikeaPuoli.pelaaja.text = ""
    }
  }
  //(peli-ikkuna)-----------------------------------------------------------------------------------------------------------
  
  
  //Päävalikko------------------------------------------------------------------------------------------------------------
  val paaValikko = new MenuBar {
    val pelaaja1 = new PelaajaMenu("Pelaaja1: Sininen")
    val pelaaja2 = new PelaajaMenu("Pelaaja2: Punainen")
    val tasovalinta = new RataMenu("Rata")
    contents += pelaaja1
    contents += pelaaja2
    contents += tasovalinta
  }
  
  //Aloittaa uuden pelin, jos asetukset ovat sallitut. Palauttaa virheilmoituksen Option-kääreessä.
  def uusiPeli(): Option[String] = {
    val valikko = this.paaValikko
    val rata = valikko.tasovalinta.valittu
    val profiili1 = valikko.pelaaja1.valittu
    val profiili2 = valikko.pelaaja2.valittu
    if ( !rata.isDefined) { //Ei pitäisi tapahtua, mutta varmuuden vuoksi tässä.
      Some("Rata pitää valita ennen pelin aloittamista.")
    } else if (profiili1.getOrElse(Profiili("eiOlemassa1")) == profiili2.getOrElse(Profiili("eiOlemassa2"))) { //Kaksi Nonea kelpaa.
      Some("Käytettävät profiilit eivät voi olla samat.")
    } else if (!rata.get.onkoEhja) {//rata on määritelty tässä vaiheessa.
      Some("Valittu rata ei sisällä lähtöruutua kaikille pelaajille. Lisää lähtöruudut rataeditorilla.")
    } else  { //Kunhan rata on määritelty ja profiilit erit
      val uusiPeli = Peli.uusiPeli(rata.get, Vector(profiili1, profiili2))
      if (profiili1.isDefined && profiili1.get.nimi == Peli.ai.nimi) uusiPeli.pelaajat(0).asetaAI(new AI(uusiPeli)) //Luodaan AI tarvittaessa
      else if (profiili2.isDefined && profiili2.get.nimi == Peli.ai.nimi) uusiPeli.pelaajat(1).asetaAI(new AI(uusiPeli))
      Ikkuna.paaIkkuna.vaihdaIkkunanSisalto(Ikkuna.peliIkkuna(uusiPeli))
      None
    }
  }
  
  def tallenaPelinTiedot(pelitilanne: Pelitilanne) = {
    val voittaja = pelitilanne.tarkistaVoitto._1
    val selitys = pelitilanne.tarkistaVoitto._2
    var kierrosajat = Map[String, Option[Int]]()
    if (voittaja.isDefined) { //Jos peli on päättynyt
      pelitilanne.pelaajat.foreach { pelaaja => 
        pelaaja.profiili.foreach{ profiili => if (profiili.nimi != Peli.ai.nimi) { //Jos profiili != Non ja != AI
          val voitti = pelaaja == voittaja.get
          val siirrot = if (pelitilanne.onkoMaalissa(pelaaja)) Some(pelaaja.auto.tehdytSiirrot) else None
          profiili.paivita(voitti, pelitilanne.pelilauta.nimi, siirrot)
          kierrosajat += (profiili.nimi -> siirrot)
        }}
      }
      pelitilanne.pelilauta.rata.paivita(kierrosajat)
    }
  }
  //(päävalikko)-----------------------------------------------------------------------------------------------------------------------

  //Rataeditori------------------------------------------------------------------------------------------------------------------------
def rataeditoriValikko: MenuBar = new MenuBar {
    val yleisvalikko = new Menu("Menu") {
      contents += new MenuItem(Action("Palaa päävalikkoon"){NappuloidenHallinta.palaaMenuun()})
    }
    val uusiRata = new Menu("Uusi rata") {
      contents += new MenuItem(Action("12 x 12"){NappuloidenHallinta.avaaUusi(12,12,maastoValikko)})
      contents += new MenuItem(Action("22 x 16"){NappuloidenHallinta.avaaUusi(22,16,maastoValikko)})
      contents += new MenuItem(Action("24 x 24"){NappuloidenHallinta.avaaUusi(24,24,maastoValikko)})
      contents += new MenuItem(Action("28 x 20"){NappuloidenHallinta.avaaUusi(28,20,maastoValikko)})
      contents += new MenuItem(Action("32 x 28"){NappuloidenHallinta.avaaUusi(32,28,maastoValikko)})
    }
    val kopioiRata = new Menu("Kopio rata") {
      val menulista: Vector[MenuItem] = Peli.rataLista.map(rata => 
        new MenuItem(Action(rata.nimi)(NappuloidenHallinta.avaaKopioRata(rata, maastoValikko)) ) )
      menulista.foreach(contents += _) //Lisätään kaikki profiilit menuun
    }
    val muokkaaRataa = new Menu("Muokkaa rataa") {
      val menulista: Vector[MenuItem] = Peli.rataLista.map(rata => 
        new MenuItem(Action(rata.nimi)(NappuloidenHallinta.avaaRata(rata, maastoValikko)) ) )
      menulista.foreach(contents += _) //Lisätään kaikki profiilit menuun
    }
    val maastoValikko = new MaastoMenu("Lisää maasto")
    contents += yleisvalikko
    contents += uusiRata
    contents += kopioiRata
    contents += muokkaaRataa
    contents += maastoValikko
  }
  
  def avaaKopioRata(rata: Rata, menu: MaastoMenu): Unit = {
    val nimi = JOptionPane.showInputDialog("Valitse radan nimi:")
    val malli = """([a-zA-Z0-9]+)""".r
    if (nimi != null) {
      if (Peli.rataLista.map(_.nimi).contains(nimi)) {
        Dialog.showMessage(null, "Nimi "+nimi+ " on jo käytössä. Valitse toinen nimi.",
                           "Virhe rataa luodessa", Dialog.Message.Error)
        avaaKopioRata(rata, menu)
      } else {
        nimi match {
          case malli(nimi) => {
            val kopio = Rata.kopio(nimi, rata)
            Peli.uusiRata(kopio)
            Ikkuna.paaIkkuna.contents = Ikkuna.rataeditori(rata, menu)
            Dialog.showMessage(null, "Uusi rata "+nimi+ " luotiin onnistuneesti.\n"+
                        "Voit nyt muokata rataa editorilla. Kaikki muutokset tallennetaan automaattisesti.","Uusi rata luotiin", Dialog.Message.Info)
          }
          case _ => {
            Dialog.showMessage(null, "Nimeä "+nimi+ " ei voida hyväksyä. Nimi saa sisältää vain aakkosia (a-z) ja numeroita.",
                           "Virhe rataa luodessa", Dialog.Message.Error)
            avaaKopioRata(rata, menu)
          }
        }
      }
    }
  }
  
  def avaaRata(rata: Rata, menu: MaastoMenu) = {
    val vastaus = Dialog.showConfirmation(null, "Kaikki muutokset tallentuvat automaattisesti, eikä niitä voi perua.\nJos et ole varma "+
                             "siitä mitä aiot tehdä, on turvallisempaa käyttää kopioitua pohjaa radasta.",
                             "Huomio!", Dialog.Options.OkCancel, Dialog.Message.Warning)
    
    if (vastaus == Dialog.Result.Ok) Ikkuna.paaIkkuna.contents = Ikkuna.rataeditori(rata, menu)
  }
  
  def avaaUusi(leveys: Int, korkeus: Int, menu: MaastoMenu): Unit = {
    val nimi = JOptionPane.showInputDialog("Valitse radan nimi:")
    val malli = """([a-zA-Z0-9]+)""".r
    if (nimi != null) {
      if (Peli.rataLista.map(_.nimi).contains(nimi)) {
        Dialog.showMessage(null, "Nimi "+nimi+ " on jo käytössä. Valitse toinen nimi.",
                           "Virhe rataa luodessa", Dialog.Message.Error)
        avaaUusi(leveys, korkeus, menu)
      } else {
        nimi match {
          case malli(nimi) => {
            val uusi = Rata.uusi(nimi, leveys, korkeus)
            Peli.uusiRata(uusi)
            Ikkuna.paaIkkuna.contents = Ikkuna.rataeditori(uusi, menu)
            Dialog.showMessage(null, "Uusi rata "+nimi+ " luotiin onnistuneesti.\n"+
                        "Voit nyt muokata rataa editorilla,\nmutta saatat joutua käynnistämään pelin uudestaan "+
                        "pystyäksesi pelaamaan sitä.\nKaikki muutokset tallentuvat automaattisesti.",
                        "Uusi rata luotiin", Dialog.Message.Info)
          }
          case _ => {
            Dialog.showMessage(null, "Nimeä "+nimi+ " ei voida hyväksyä. Nimi saa sisältää vain aakkosia (a-z) ja numeroita.",
                           "Virhe rataa luodessa", Dialog.Message.Error)
            avaaUusi(leveys, korkeus, menu)
          }
        }
      }
    }
  }
  //(Rataeditori)-----------------------------------------------------------------------------------------------------------------------
  
  
  //Profiilit ja radat-----------------------------------------------------------------------------------------------------------------
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
    def rataMenu = new Menu("Ratojen tiedot") {
      val menulista: Vector[MenuItem] = Peli.rataLista.map(rata => 
        new MenuItem(Action(rata.nimi)(NappuloidenHallinta.haeRadanTiedot(rata)) ) )
      menulista.foreach(contents += _) //Lisätään kaikki profiilit menuun
    }
    contents += profiiliMenu
    contents += rataMenu
  }
  
  def haeProfiilienTiedot(profiili: Profiili) {
    Ikkuna.paaIkkuna.contents = Ikkuna.profiilienHallinta(profiili)
  }
  
  def haeRadanTiedot(rata: Rata) {
    Ikkuna.paaIkkuna.contents = Ikkuna.rataTiedot(rata)
  }
  
  //Kysyy käyttää syöttämään luotavan profiilin nimen. Ei hyväksy erikoismerkkejä sisältäviä nimiä eikä aiemmin luodun profiilin nimeä.
  def luoUusiProfiili() {
    val nimi = JOptionPane.showInputDialog("Valitse profiilin nimi:")
    val malli = """([a-zA-Z0-9]+)""".r
    if (nimi != null) {
      if (Peli.profiiliLista.map(_.nimi).contains(nimi)) {
        Dialog.showMessage(null, "Nimi "+nimi+ " on jo käytössä. Valitse toinen nimi.",
                           "Virhe profiilia luodessa", Dialog.Message.Error)
        luoUusiProfiili()
      } else {
        nimi match {
          case malli(nimi) => {
            Peli.uusiProfiili(Profiili(nimi))
            Dialog.showMessage(null, "Uusi profiili "+nimi+ " luotiin onnistuneesti.\n"+
                        "Voit kuitenkin joutua käynnistämään peli-ikkunnan uudestaan, "+
                        "jotta muutokset päivittyvät", "Uusi pelaaja luotiin", Dialog.Message.Info)
          }
          case _ => {
            Dialog.showMessage(null, "Nimeä "+nimi+ " ei voida hyväksyä. Nimi saa sisältää vain aakkosia (a-z) ja numeroita.",
                           "Virhe profiilia luodessa", Dialog.Message.Error)
            luoUusiProfiili()
          }
        }
      }
    }
  }
  
  //Tämän funktion ainut tarkoitus on kiertää rekursiivinen viittaus, joka syntyi, kun profiilivalikossa kutsuttiin tätä metodia.
  def palaaMenuun() {
    Ikkuna.paaIkkuna.vaihdaIkkunanSisaltoMenuun()
  }
  //(profiilit ja radat)--------------------------------------------------------------------------------------------------------------
  
  //Ohjeet---------------------------------------------------------------------------------------------
  def ohjeetValikko = new MenuBar {
    contents += new Menu("Menu") {
      contents += new MenuItem(Action("Palaa päävalikkoon"){NappuloidenHallinta.palaaMenuun()})
    }
  }
  //(ohjeet)--------------------------------------------------------------------------------------------
  
  
}
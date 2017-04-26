

package gui

import peli.{Peli, Pelitilanne}
import tietojenTallennus.{Profiili, Rata}

import scala.swing._
import scala.swing.event._
import javax.swing.{UIManager}


/* Lähteet:
 * Pentaminoes-projektin (Ryhmämme työ Ohjelmointi 1 kurssilla) GameWindow-olio 
 * Ohjelmointi 1 kurssin materiaalin Swing-esimerkkipeli: sekä pdf että kooditiedostoista View-olio
 * 
 * BoxPanel(Orientation.Vertical) löydetty: http://stackoverflow.com/questions/13380701/scala-swing-component-alignment
 * 
 * case match ongelma korjattu: http://alvinalexander.com/scala/scala-unreachable-code-due-to-variable-pattern-message
 *///http://alvinalexander.com/java/jwarehouse/scala/src/swing/scala/swing/test/UIDemo.scala.shtml


object Ikkuna extends SimpleSwingApplication {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  
  val leveys = 1000  
  val korkeus = 800
  
  //PeliIkkuna-------------------------------------------------------------------
  val vaihteenVaihto = new BoxPanel(Orientation.Horizontal) {
  
    val vaihdeYlos = new Button {
      preferredSize = new Dimension(100,100)
      text = "->" 
    }
    val vaihdeAlas = new Button {
      preferredSize = new Dimension(100,100)
      text = "<-"
    }
    val vaihdeLuku = new Button {
      preferredSize = new Dimension(100,100)
      text = "1"
    }
    
    this.contents += vaihdeAlas
    this.contents += vaihdeLuku
    this.contents += vaihdeYlos

  }
  
  def vasenPuoli(pelitilanne: Pelitilanne) = new BoxPanel(Orientation.Vertical) {
    this.contents += new Ruudukko(pelitilanne)
    this.contents += vaihteenVaihto
  }
  
  val peruSiirto = new Button {
      preferredSize = new Dimension(100,50)
      text = "Peru siirto"
    }
    val palaaMenuun = new Button {
      preferredSize = new Dimension(100,50)
      text = "Lopeta peli" 
    }
  
  val oikeaPuoli = new BoxPanel(Orientation.Vertical) {
    val vuorossa = new Label{text="Vuorossa:"; font=new Font("Arial",0,24)}
    val vari = new Label{font=new Font("Arial",0,24)}
    val pelaaja = new Label{font=new Font("Arial",0,24)}
    contents += vuorossa
    contents += vari
    contents += pelaaja
    contents += peruSiirto
    contents += palaaMenuun

  }
  
  def peliIkkuna(pelitilanne: Pelitilanne) = new GridBagPanel {
    val c = new Constraints
    c.insets = new Insets(50,50,50,50)
    layout(vasenPuoli(pelitilanne)) = c
    c.gridx = 1
    layout(oikeaPuoli) = c
  }
  //(peliIkkuna)-----------------------------------------------------
  
  
  //Menu-----------------------------------------------------------------------
  val menu = new GridBagPanel {
    val uusiPeli = new Button {
      preferredSize = new Dimension(200,200)
      text = "Uusi peli" 
    }
    val ohjeet = new Button {
      preferredSize = new Dimension(200,200)
      text = "Ohjeet"
    }
    val rataeditori = new Button {
      preferredSize = new Dimension(200,200)
      text = "Rataeditori" 
    }
    val profiilit = new Button {
      preferredSize = new Dimension(200,200)
      text = "Profiilit ja radat"
    }
    
    val c = new Constraints
    c.grid = (2,2)
    c.insets = new Insets(50,50,50,50)
    c.gridx = 0 //Uusipeli-nappula
    c.gridy = 0
    layout(uusiPeli) = c
    c.gridx = 1 //Profiilit-nappula
    c.gridy = 0
    layout(profiilit) = c
    c.gridx = 0 //Rataeditori-nappula
    c.gridy = 1
    layout(rataeditori) = c
    c.gridx = 1 //Lopeta-nappul
    c.gridy = 1
    layout(ohjeet) = c
  }
  //(menu)---------------------------------------------------------------------------------------------
  
  //Profiilienhallinta-------------------------------------------------------------------------------------
  def profiilienHallinta(profiili: Profiili) = new GridBagPanel {
    val c = new Constraints
    c.gridx = 0
    c.gridy = 0
    c.ipady = 25
    val tiedot = profiili.tiedot.toList
    val alku = 
      if (tiedot.isEmpty) List(profiili.nimi, "", "Ei ole pelannut yhtään peliä.")
      else List(profiili.nimi, "", "Rata | Voitot | Pelit | Ennätysaika")

    val kaikki = (alku ++ tiedot).toVector
    for (rivi <- kaikki) {
      c.gridy += 1
      layout(new Label{text=rivi; font=new Font("Arial",0,40)}) = c
    }
  }
  //(profiilienhallina)--------------------------------------------------------------------------------------
  
  //Ratojen tilastot-----------------------------------------------------------------------------------------
  def rataTiedot(rata: Rata) = new GridBagPanel {
    val c = new Constraints
    c.gridx = 0
    c.gridy = 0
    c.ipady = 25
    val tiedot = rata.parhaatAjajatTeksti
    val alku = 
      if (tiedot.isEmpty) List(rata.nimi, "", "Rataa ei ole läpäisty kertaakaan.")
      else List(rata.nimi, "", "Radan ennätysajat:", "Nimi | Siirtojen määrä")

    val kaikki = (alku ++ tiedot).toVector
    for (rivi <- kaikki) {
      c.gridy += 1
      layout(new Label{text=rivi; font=new Font("Arial",0,40)}) = c
    }
  }
  //(Ratojen tilastot)-----------------------------------------------------------------------------------------
  
  //Rataeditori------------------------------------------------------------------------------------------------
  def rataeditori(rata: Rata, menu: MaastoMenu) = new BoxPanel(Orientation.Vertical) {
    val radanNimi = new Label{text="Muokattava rata: "+rata.nimi; font=new Font("Arial",0,40)}
    val ruudukko = new GridBagPanel {
      val c = new Constraints
      layout(new Rataeditori(rata, menu)) = c
    }
    contents += radanNimi
    contents += ruudukko
  }
  
  
  //(Rataeditori)----------------------------------------------------------------------------------------------
  
  //Ohjeet-----------------------------------------------------------------------------------------------------
  val ohjeet = new GridBagPanel {
    val c = new Constraints
    val teksti = new TextArea(20,50) {
      font = new Font("Arial",0,20)
      text = "Pelin tarkoitus on kiertää lauta vastustajan autoa (kuvataan ympyrällä) nopeammin. Pelaaja joka ylittää "+
             "maaliviivan ensimmäisenä voittaa pelin. " +
             "Jos pelaajat ylittävät viivan samalla kierroksella, voittaa toisena pelaava. Pelin voi myös hävitä, jos pelaajalla ei "+
             "ole enää mahdollisuutta tehdä laillista siirtoa.\n\n" +
             "Pelaaja voi halutessaan nostaa tai laskea vaihdetta yhdellä jokaisella vuorollaan ja sitten valita vaihteen mukaisista " +
             "kolmesta vaihtoehdosta siirtonsa. Siirtovaihtoehdot määräytyvät sen suunnan mukaan, johon autolla edellisellä siirrolla " +
             "ajettiin. Mahdolliset siirrot ovat merkitty isoilla neliöillä.\n\n" +
             "Siirtovaihtoehtoja saattavat rajoittaa eri maastot joita pelissä esiintyy. Kaksi tavallisinta maastotyyppia ovat normaali "+
             "TIE (valkoinen) ja radan REUNA (musta). Tie ei rajoita ajamista mitenkään, mutta reunan yli taas ei saa ajaa. "+
             "Muita maastotyyppijä ovat:\n\n"+
             "HIEKKA (keltainen): estää siinä ajavaa autoa nostamasta vaihdetta.\n"+
             "SYVÄ HIEKKA (ruskea): pakottaa pienentämään vaihdetta. Jos auton vaihde laskee nollaan, pelaaja häviää.\n"+
             "JÄÄ (vaaleansininen): jään yli ajavan siirron on pakko olla suuremmalla vaihteella kuin edellinen siirto tai "+
             "samalla vaihteella ja samaan suuntaan kuin edellinnen siirto.\n" +
             "ÖLJY (tumman lila): öljyn yli ajava siirto ei voi olla samaan suuntaan kuin edellinen siirto.\n"+
             "MAALI (harmaa): maaleja on neljää eri tyyppiä, vaikka se ei väristä näykään. Maalin tyyppi ratkaisee sen " +
             "mihin suuntaan maalin läpi pitää ajaa eli kierretäänkö rataa myötä vai vastapäivään ja onko maaliviiva pysty- vai vaakasuora."
      wordWrap = true
      lineWrap = true
      editable = false
      opaque = false
    }
    layout(teksti) = c
  }
  //(ohjeet)---------------------------------------------------------------------------------------------------
  
  val paaIkkuna = new MainFrame {
    title = "Formula"
    preferredSize = new Dimension(leveys, korkeus)

    resizable = false
    
    menuBar = NappuloidenHallinta.paaValikko
    contents = menu

    def vaihdaIkkunanSisalto(sisalto: GridBagPanel) = {
      this.contents = sisalto
      this.repaint()
    }
    
    def vaihdaIkkunanSisaltoMenuun() = {
      this.menuBar = NappuloidenHallinta.paaValikko
      this.vaihdaIkkunanSisalto(menu)
    } 
    def vaihdaIkkunanSisaltoPeliin() = {
      val virheIlmoitus = NappuloidenHallinta.uusiPeli() //Menua ei voi vaihtaa ennen kuin pelin tiedot on ladattu.
      if (virheIlmoitus.isDefined) Dialog.showMessage(null, virheIlmoitus.get, "Virhe asetuksissa", Dialog.Message.Error)
      else this.menuBar = new MenuBar()
    }
    def vaihdaIkkunanSisaltoProfiilienHallintaan() = {
      this.menuBar = NappuloidenHallinta.profiiliValikko
      this.vaihdaIkkunanSisalto(new GridBagPanel)
    }
    def vaihdaIkkunanSisaltoRataeditoriin() = {
      this.menuBar = NappuloidenHallinta.rataeditoriValikko
      this.vaihdaIkkunanSisalto(new GridBagPanel)
    }
    def vaihdaIkkunanSisaltoOhjeisiin() = {
      this.menuBar = NappuloidenHallinta.ohjeetValikko
      this.vaihdaIkkunanSisalto(ohjeet)
    }
    
    listenTo(vaihteenVaihto.vaihdeYlos, vaihteenVaihto.vaihdeAlas,
             peruSiirto, palaaMenuun,
             menu.uusiPeli, menu.profiilit, menu.rataeditori, menu.ohjeet)
    reactions += {
      case ButtonClicked(nappula) => {
        if (nappula == vaihteenVaihto.vaihdeYlos) NappuloidenHallinta.nostaVaihdetta()
        else if (nappula == vaihteenVaihto.vaihdeAlas) NappuloidenHallinta.laskeVaihdetta()
        else if (nappula == peruSiirto) NappuloidenHallinta.peruEdellinenSiirto()
        else if (nappula == palaaMenuun) NappuloidenHallinta.lopetaPeli()
        else if (nappula == menu.uusiPeli) this.vaihdaIkkunanSisaltoPeliin()
        else if (nappula == menu.profiilit) this.vaihdaIkkunanSisaltoProfiilienHallintaan()
        else if (nappula == menu.rataeditori) this.vaihdaIkkunanSisaltoRataeditoriin()
        else if (nappula == menu.ohjeet) this.vaihdaIkkunanSisaltoOhjeisiin()
        NappuloidenHallinta.paivita() //Nappuloiden painamisen jälkeen päivitetään nappuoiden tekstit (oleellista lähinnä pelin aikana)
        this.repaint() // ja repaintataan ruutu
      }
    }
  }
  
  def top = paaIkkuna
    
}


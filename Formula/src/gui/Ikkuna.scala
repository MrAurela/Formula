
package gui

import peli.Peli

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
 */



object Ikkuna extends SimpleSwingApplication {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  
  val leveys = 1000
  val korkeus = 800
  
  val ruudukko = new Ruudukko(Peli.pelitilanne.get.pelilauta.leveys, Peli.pelitilanne.get.pelilauta.korkeus)
  
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
  
  val vasenPuoli = new BoxPanel(Orientation.Vertical) {
    this.contents += ruudukko
    this.contents += vaihteenVaihto
  }
  
  val oikeaPuoli = new BoxPanel(Orientation.Vertical) {
    val peruSiirto = new Button {
      preferredSize = new Dimension(100,50)
      text = "Peru siirto"
    }
    val palaaMenuun = new Button {
      preferredSize = new Dimension(100,50)
      text = "Lopeta peli" 
    }
    
    contents += peruSiirto
    contents += palaaMenuun

  }
  
  val peliIkkuna = new GridBagPanel {
    val c = new Constraints
    c.insets = new Insets(50,50,50,50)
    layout(vasenPuoli) = c
    c.gridx = 1
    layout(oikeaPuoli) = c
  }
  
  
  //Menu
  val menu = new GridBagPanel {
    val uusiPeli = new Button {
      preferredSize = new Dimension(200,200)
      text = "Uusi peli" 
    }
    val lopetaPeli = new Button {
      preferredSize = new Dimension(200,200)
      text = "Lopeta" 
    }
    val rataeditori = new Button {
      preferredSize = new Dimension(200,200)
      text = "Rataeditori" 
    }
    val profiilit = new Button {
      preferredSize = new Dimension(200,200)
      text = "Profiilit"
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
    layout(lopetaPeli) = c
  }
  //(menu)---------------------------------------------------------------------------------------------
  
  
  
  /*val paaValikko = new MenuBar {
    contents += new Menu("Menu") {
      contents += new MenuItem(Action("Uusi peli")(newGame))
    }
  }*/

  val paaIkkuna = new MainFrame {
    title = "Formula"
    preferredSize = new Dimension(leveys, korkeus)

    contents = menu

    def vaihdaIkkunanSisalto(sisalto: GridBagPanel) = {
      this.contents = sisalto
      this.repaint()
    }
    
    //Tarvitaan vain jos käytetään näppäimistöä
    //gameScreen.requestFocus
    
    listenTo(vaihteenVaihto.vaihdeYlos, vaihteenVaihto.vaihdeAlas,
             oikeaPuoli.peruSiirto, oikeaPuoli.palaaMenuun,
             menu.uusiPeli, menu.profiilit, menu.rataeditori, menu.lopetaPeli)
    reactions += {
      case ButtonClicked(nappula) => {
        if (nappula == vaihteenVaihto.vaihdeYlos) NappuloidenHallinta.nostaVaihdetta()
        else if (nappula == vaihteenVaihto.vaihdeAlas) NappuloidenHallinta.laskeVaihdetta()
        else if (nappula == oikeaPuoli.peruSiirto) NappuloidenHallinta.peruEdellinenSiirto()
        else if (nappula == oikeaPuoli.palaaMenuun) this.vaihdaIkkunanSisalto(menu)
        else if (nappula == menu.uusiPeli) this.vaihdaIkkunanSisalto(peliIkkuna)
        else if (nappula == menu.profiilit) println("Profiilin hallintaaa ei ole toteutettu vielä.")
        else if (nappula == menu.rataeditori) println("Rataeditoria ei ole toteutettu vielä.")
        else if (nappula == menu.lopetaPeli) this.dispose()
        NappuloidenHallinta.paivita() //Nappuloiden painamisen jälkeen päivitetään nappuoiden tekstit ja repaintataan ruutu
        this.repaint()
      }
    }
  }
  
  def top = paaIkkuna
  NappuloidenHallinta.paivita()
    
}


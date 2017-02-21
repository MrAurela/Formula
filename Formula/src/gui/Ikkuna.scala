
package gui

import peli.Peli

import scala.swing._
import javax.swing._


/* Lähteet:
 * Pentaminoes-projektin (Ryhmämme työ Ohjelmointi 1 kurssilla) GameWindow-olio 
 * Ohjelmointi 1 kurssin materiaalin Swing-esimerkkipeli: sekä pdf että kooditiedostoista View-olio
 * 
 * BoxPanel(Orientation.Vertical) löydetty: http://stackoverflow.com/questions/13380701/scala-swing-component-alignment
 */



object Ikkuna extends SimpleSwingApplication {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  
  val leveys = 1000
  val korkeus = 800
  
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
    var ruudukko = new Ruudukko(5, 5)
    this.contents += ruudukko
    this.contents += vaihteenVaihto
  }
  
  val ikkunanSisalto = new GridBagPanel {
    val c = new Constraints
    c.insets = new Insets(50,50,50,50)
    layout(vasenPuoli) = c
  }
  
  def top = new MainFrame {
    title = "Formula"
    preferredSize = new Dimension(leveys, korkeus)
    
    contents = ikkunanSisalto
    //Tarvitaan vain jos käytetään näppäimistöä
    //gameScreen.requestFocus
  }
    
    
}


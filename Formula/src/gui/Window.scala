
package gui

import scala.swing._
import javax.swing._

/* Lähteet:
 * Pentaminoes-projektin (Ryhmämme työ Ohjelmointi 1 kurssilla) GameWindow-olio 
 * Ohjelmointi 1 kurssin materiaalin Swing-esimerkkipeli: sekä pdf että kooditiedostoista View-olio
 */



object Window extends SimpleSwingApplication {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  
  val width = 640
  val height = 480
  
  
  val gameScreen = new GridBagPanel {
    ???
  }
  
  def top = new MainFrame {
    title = "Formula"
    preferredSize = new Dimension(width, height)
    
    contents = gameScreen
    gameScreen.requestFocus
    
    

  }
}
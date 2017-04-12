package gui

import peli.Peli
import scala.swing._

//Menu (radiobutton) - http://alvinalexander.com/java/jwarehouse/scala/src/swing/scala/swing/test/UIDemo.scala.shtml
// -||- https://lampsvn.epfl.ch/trac/scala/browser/scala/trunk/src/swing/scala/swing/test/Dialogs.scala?rev=25699#L54

case class PelaajaMenu(nimi: String) extends Menu(nimi) {
  

  val menulista = Peli.profiiliLista.map(profiili => new RadioMenuItem(profiili.nimi))
  val profiilit = menulista.zip(Peli.profiiliLista).toMap //saadaan profiili nimen perusteella
                                    
  var vaihtoehdot = new ButtonGroup() //Luodaan radiobox ryhmä
  val eiProfiilia = new RadioMenuItem("EI PROFIILIA")
  vaihtoehdot.buttons += eiProfiilia
  vaihtoehdot.buttons ++= menulista
  
  contents ++= vaihtoehdot.buttons //Lisätään menuun vaihtoehdot
  
  vaihtoehdot.select(eiProfiilia)
  
}


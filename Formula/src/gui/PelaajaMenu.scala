package gui

import peli.Peli
import tietojenTallennus.Profiili
import scala.swing._

//Menu (radiobutton) - http://alvinalexander.com/java/jwarehouse/scala/src/swing/scala/swing/test/UIDemo.scala.shtml
// -||- https://lampsvn.epfl.ch/trac/scala/browser/scala/trunk/src/swing/scala/swing/test/Dialogs.scala?rev=25699#L54

class PelaajaMenu(nimi: String) extends Menu(nimi) {
  
  val menulista = Peli.profiiliLista.map(profiili => new RadioMenuItem(profiili.nimi)) 
  var profiilit = menulista.zip(Peli.profiiliLista).toMap //saadaan profiili nimen perusteella

  var vaihtoehdot = new ButtonGroup() //Luodaan radiobox ryhmä
  val eiProfiilia = new RadioMenuItem("EI PROFIILIA")
  val ai = new RadioMenuItem(Peli.ai.nimi)
  
  vaihtoehdot.buttons += eiProfiilia
  vaihtoehdot.buttons += ai
  vaihtoehdot.buttons ++= menulista
  
  contents += eiProfiilia//Lisätään menuun vaihtoehdot
  contents += ai
  contents += new Separator
  contents ++= menulista.sortWith{(menu1, menu2) => menu1.text < menu2.text} //Lisätään aakkosjärjestyksessä
  
  vaihtoehdot.select(eiProfiilia)
  
  def valittu: Option[Profiili] = {
    if (ai.selected) Some(Peli.ai)
    else this.profiilit.get(this.menulista.find(_.selected).getOrElse(new RadioMenuItem("")) )
  }
  
}


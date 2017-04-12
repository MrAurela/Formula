package gui

import peli.Peli
import scala.swing._

//Menu (radiobutton) - http://alvinalexander.com/java/jwarehouse/scala/src/swing/scala/swing/test/UIDemo.scala.shtml
// -||- https://lampsvn.epfl.ch/trac/scala/browser/scala/trunk/src/swing/scala/swing/test/Dialogs.scala?rev=25699#L54

case class RataMenu(nimi: String) extends Menu(nimi) {
  

  val menulista = Peli.rataLista.map(rata => new RadioMenuItem(rata.nimi))
  val radat = menulista.zip(Peli.rataLista).toMap //yhdistetään nappulat ratoihin
  
  var vaihtoehdot = new ButtonGroup() //Luodaan radiobox ryhmä
  vaihtoehdot.buttons ++= menulista
  
  contents ++= vaihtoehdot.buttons //Lisätään menuun vaihtoehdot
  
  if ( !menulista.isEmpty) vaihtoehdot.select(menulista(0))
  
}


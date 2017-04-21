package gui

import pelikomponentit.Maasto
import scala.swing._
import tietojenTallennus.Rata

class MaastoMenu(nimi: String) extends Menu(nimi) {
  val nimistaMaastoihin = Maasto.maastoTunnukset.map(Maasto(_)).map(maasto => (new RadioMenuItem(maasto.nimi), maasto)).toMap
  
  var vaihtoehdot = new ButtonGroup() //Luodaan radiobox ryhmä
  vaihtoehdot.buttons ++= nimistaMaastoihin.keySet
  
  //Lisätään vaihtoehdot aakkosjärjestyksessä.
  contents ++= vaihtoehdot.buttons.toArray.sortWith{(menuMaasto1,menuMaasto2) => menuMaasto1.text < menuMaasto2.text}
  
  if ( !vaihtoehdot.buttons.isEmpty) vaihtoehdot.select(vaihtoehdot.buttons.toList(0))
  
  def valittu: Option[Maasto] = {
    this.nimistaMaastoihin.get( this.nimistaMaastoihin.keySet.find(_.selected).getOrElse(new RadioMenuItem("")) )
  }
  
}
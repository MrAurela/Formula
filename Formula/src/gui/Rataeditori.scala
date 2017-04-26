package gui

import pelikomponentit._
import siirrot.Koordinaatti
import tietojenTallennus.Rata

import scala.swing._
import scala.swing.event._
import java.awt.Color
import scala.swing.Dialog

//Suurimmilta osiltaan rakennettu Ruudukko-luokan pohjalta, joten luokissa on toisteisuutta.

class Rataeditori(rata: Rata, menu: MaastoMenu) extends Panel {
  
  val ruudunKoko = 20
  val vaakaRuudut = rata.leveys
  val pystyRuudut = rata.korkeus
  
  preferredSize = new Dimension(vaakaRuudut * ruudunKoko +1, pystyRuudut * ruudunKoko +1) //+1, jottei reunimmaisimmat viivat leikkaudu pois.
  
  override def paintComponent(g: Graphics2D) = {
    this.piirraMaastot(g)
    this.piirraRuudukko(g)
  }
  
  private def piirraRuudukko(g: Graphics2D) = { //Piirtää ruudukon viivat
    g.setColor(Color.BLACK)
    for (vaaka <- 0 to vaakaRuudut) g.drawLine(vaaka * ruudunKoko, 0, vaaka * ruudunKoko, pystyRuudut * ruudunKoko)
    for (pysty <- 0 to pystyRuudut) g.drawLine(0, pysty * ruudunKoko, vaakaRuudut * ruudunKoko, pysty * ruudunKoko)
  }
  
  private def piirraMaastot(g: Graphics2D) = { //Piirtää maastoruudut eri väreillä
    val ruudut = rata.muoto
    for (vaaka <- 0 until vaakaRuudut; pysty <- 0 until pystyRuudut) {
      ruudut(pysty)(vaaka) match { //Valitaan maaston väri
        case Jaa => g.setColor(Color.CYAN)
        case Hiekka => g.setColor(new Color(232,255,56)) // 232,255,56 => keltainen
        case SyvaHiekka => g.setColor(new Color(175,55,4)) // 175,55,4 => ruskea
        case Oljy => g.setColor(new Color(95,2,92)) // 95,2,92 => Tumman lila
        case Tie(_,_) => g.setColor(Color.WHITE)
        case Reuna => g.setColor(Color.BLACK)
        case Maali(_,_) => g.setColor(Color.GRAY)
      }
      g.fillRect(vaaka * ruudunKoko, pysty * ruudunKoko, ruudunKoko, ruudunKoko) //Piirretään maastoruutu
    }
  }
  
  listenTo(this.mouse.clicks)
    reactions += {
      case MouseClicked(ruudukko, sijainti, _, _, _) => {
        val koordinaatti = Koordinaatti(sijainti.x / ruudunKoko, sijainti.y  / ruudunKoko)
        menu.valittu.foreach { valittu => rata.muutaMaasto(koordinaatti.y, koordinaatti.x, valittu) }
        rata.paivita(Map())
        repaint()
      }
    }
  
  
}
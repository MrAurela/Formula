package gui

import peli.Peli
import pelikomponentit._

import scala.swing._
import java.awt.Color

class Ruudukko(vaakaRuudut: Int, pystyRuudut: Int) extends Panel {
  
  var ruudunKoko = 50
  
  preferredSize = new Dimension(pystyRuudut * ruudunKoko + 1, vaakaRuudut * ruudunKoko + 1 )
  
  override def paintComponent(g: Graphics2D) = {
    this.piirraMaastotJaAutot(g)
    this.piirraRuudukko(g)
  }
  
  private def piirraRuudukko(g: Graphics2D) = { //Piirtää ruudukon viivat
    g.setColor(Color.BLACK)
    for (vaaka <- 0 to vaakaRuudut) g.drawLine(vaaka * ruudunKoko, 0, vaaka * ruudunKoko, pystyRuudut * ruudunKoko)
    for (pysty <- 0 to pystyRuudut) g.drawLine(0, pysty * ruudunKoko, vaakaRuudut * ruudunKoko, pysty * ruudunKoko)
  }
  
  private def piirraMaastotJaAutot(g: Graphics2D) = { //Piirtää maastoruudut eri väreillä sekä autot
    val ruudut = Peli.pelitilanne.get.pelilauta.ruudut
    for (vaaka <- 0 until vaakaRuudut; pysty <- 0 until pystyRuudut) {
      ruudut(vaaka)(pysty).maasto match { //Valitaan maaston väri
        case Rata => g.setColor(Color.WHITE)
        case Reuna => g.setColor(Color.BLACK)
        case Maali(_) => g.setColor(Color.GRAY)
      }
      g.fillRect(vaaka * ruudunKoko, pysty * ruudunKoko, ruudunKoko, ruudunKoko) //Piirretään maastoruutu
      
      if (ruudut(vaaka)(pysty).onAuto) { //Jos ruudussa on auto
        g.setColor(Color.BLUE)
        g.fillOval(((vaaka + 0.25) * ruudunKoko).toInt , ((pysty + 0.25) * ruudunKoko).toInt, //Piirretään auto
            (0.5*ruudunKoko).toInt, (0.5*ruudunKoko).toInt)
      }
    }
  }
}
package gui

import peli.Peli
import pelikomponentit._

import scala.swing._
import java.awt.Color

class Ruudukko(vaakaRuudut: Int, pystyRuudut: Int) extends Panel {
  
  var ruudunKoko = 50
  
  preferredSize = new Dimension(pystyRuudut * ruudunKoko + 1, vaakaRuudut * ruudunKoko + 1 )
  
  override def paintComponent(g: Graphics2D) = {
    this.drawTerrains(g)
    this.drawLines(g)
  }
  
  private def drawLines(g: Graphics2D) = {
    g.setColor(Color.BLACK)
    for (vaaka <- 0 to vaakaRuudut) g.drawLine(vaaka * ruudunKoko, 0, vaaka * ruudunKoko, pystyRuudut * ruudunKoko)
    for (pysty <- 0 to pystyRuudut) g.drawLine(0, pysty * ruudunKoko, vaakaRuudut * ruudunKoko, pysty * ruudunKoko)
  }
  private def drawTerrains(g: Graphics2D) = {
    val ruudut = Peli.pelitilanne.get.pelilauta.ruudut
    for (vaaka <- 0 until vaakaRuudut; pysty <- 0 until pystyRuudut) {
      ruudut(vaaka)(pysty).maasto match {
        case Rata => g.setColor(Color.BLACK)
        case Reuna => g.setColor(Color.WHITE)
      }
      g.fillRect(vaaka * ruudunKoko, pysty * ruudunKoko, ruudunKoko, ruudunKoko)
      if (ruudut(vaaka)(pysty).onAuto) {
        g.setColor(Color.BLUE)
        g.fillOval(((vaaka + 0.25) * ruudunKoko).toInt , ((pysty + 0.25) * ruudunKoko).toInt,
            (0.5*ruudunKoko).toInt, (0.5*ruudunKoko).toInt)
      }
    }
  }
}
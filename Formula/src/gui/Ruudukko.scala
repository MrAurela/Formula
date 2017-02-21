package gui

import scala.swing._

class Ruudukko(cols: Int, rows: Int) extends Panel {
  
  var ruudunKoko = 50
  
  preferredSize = new Dimension(rows * ruudunKoko + 1, cols * ruudunKoko + 1 )
  
  override def paintComponent(g: Graphics2D) = {
    this.drawLines(g)
    this.drawTerrains(g)
  }
  
  private def drawLines(g: Graphics2D) = {
    for (col <- 0 to cols) g.drawLine(col * ruudunKoko, 0, col * ruudunKoko, rows * ruudunKoko)
    for (row <- 0 to rows) g.drawLine(0, row * ruudunKoko, cols * ruudunKoko, row * ruudunKoko)
  }
  private def drawTerrains(g: Graphics2D) = println(cols, rows)
}
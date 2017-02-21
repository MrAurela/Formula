package gui

import scala.swing._

class Ruudukko(cols: Int, rows: Int) extends Panel {
  
  var squareSize = 50
  
  preferredSize = new Dimension(rows * squareSize + 1, cols * squareSize + 1 )
  
  override def paintComponent(g: Graphics2D) = {
    this.drawLines(g)
    this.drawTerrains(g)
  }
  
  private def drawLines(g: Graphics2D) = {
    for (col <- 0 to cols) g.drawLine(col * squareSize, 0, col * squareSize, rows * squareSize)
    for (row <- 0 to rows) g.drawLine(0, row * squareSize, cols * squareSize, row * squareSize)
  }
  private def drawTerrains(g: Graphics2D) = println(cols, rows)
}
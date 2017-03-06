package siirrot

import pelikomponentit.Pelilauta

case class Koordinaatti(x: Int, y: Int) {
  
  override def toString() = "( "+x+" , "+y+" )"
  
  def onLaudalla(pelilauta: Pelilauta) = x >= 0 && y >= 0 && x < pelilauta.leveys && y < pelilauta.korkeus
  
  def +(toinen: Koordinaatti) = Koordinaatti(this.x+toinen.x, this.y+toinen.y)
  def -(toinen: Koordinaatti) = Koordinaatti(this.x-toinen.x, this.y-toinen.y)
  
}
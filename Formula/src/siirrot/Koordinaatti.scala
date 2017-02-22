package siirrot

import pelikomponentit.Pelilauta

case class Koordinaatti(y: Int, x: Int) {
  
  def onLaudalla(pelilauta: Pelilauta) = x >= 0 && y >= 0 && x <= pelilauta.leveys && y <= pelilauta.korkeus
  
}
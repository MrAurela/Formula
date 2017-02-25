package gui

import peli.Peli
import pelikomponentit._
import peli.Pelitilanne
import siirrot.Koordinaatti

import scala.swing._
import scala.swing.event._
import java.awt.Color

/* Lähteet:
 * Samoja lähteitä kuin Ikkunassa.
 * 
 */

class Ruudukko(vaakaRuudut_ : Int, pystyRuudut_ : Int) extends Panel {
  
  var ruudunKoko = 20
  val vaakaRuudut = vaakaRuudut_
  val pystyRuudut = pystyRuudut_
  
  preferredSize = new Dimension(vaakaRuudut * ruudunKoko +1, pystyRuudut * ruudunKoko +1) //+1, jottei reunimmaisimmat viivat leikkaudu pois.
  
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
      ruudut(pysty)(vaaka).maasto match { //Valitaan maaston väri
        case Tie => g.setColor(Color.WHITE)
        case Reuna => g.setColor(Color.BLACK)
        case Maali(_) => g.setColor(Color.GRAY)
      }
      g.fillRect(vaaka * ruudunKoko, pysty * ruudunKoko, ruudunKoko, ruudunKoko) //Piirretään maastoruutu
      
      if (ruudut(pysty)(vaaka).onAuto) { //Jos ruudussa on auto
        g.setColor(Color.BLUE)
        g.fillOval(((vaaka + 0.25) * ruudunKoko).toInt , ((pysty + 0.25) * ruudunKoko).toInt, //Piirretään auto
            (0.5*ruudunKoko).toInt, (0.5*ruudunKoko).toInt)
      }
    }
  }
  
  listenTo(this.mouse.clicks)
    reactions += {
      case MouseClicked(ruudukko, sijainti, _, _, _) => {
        val klikattuKoordinaatti = Koordinaatti(sijainti.x / ruudunKoko, sijainti.y  / ruudunKoko)
        println(klikattuKoordinaatti)
        Peli.pelitilanne.getOrElse(Pelitilanne()).siirraAutoa(klikattuKoordinaatti)
        repaint()
        
        //Jos siirto onnistui ja vuoro vaihtui, päivitetään vaihdelaatikon teksti
        VaihteenHallinta.paivita()
      }
    }
  
  
}
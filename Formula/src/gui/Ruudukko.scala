package gui

import peli.Peli
import pelikomponentit._
import peli.Pelitilanne
import siirrot.Koordinaatti

import scala.swing._
import scala.swing.event._
import java.awt.Color
import scala.swing.Dialog

/* Lähteet:
 * Samoja lähteitä kuin Ikkunassa.
 * http://www.scala-lang.org/api/rc2/scala/swing/Dialog$.html
 * värit: http://colorizer.org/
 * värien toiminta: http://www.users.csbsju.edu/~lziegler/apidocs/java.awt.Color.html
 */

class Ruudukko(pelitilanne: Pelitilanne) extends Panel {
  
  val ruudunKoko = 20
  val vaakaRuudut = pelitilanne.pelilauta.leveys
  val pystyRuudut = pelitilanne.pelilauta.korkeus
  
  preferredSize = new Dimension(vaakaRuudut * ruudunKoko +1, pystyRuudut * ruudunKoko +1) //+1, jottei reunimmaisimmat viivat leikkaudu pois.
  
  override def paintComponent(g: Graphics2D) = {
    this.piirraMaastot(g)
    this.piirraVaihtoehdot(g)
    this.piirraAutot(g)
    this.piirraRuudukko(g)
  }
  
  private def piirraRuudukko(g: Graphics2D) = { //Piirtää ruudukon viivat
    g.setColor(Color.BLACK)
    for (vaaka <- 0 to vaakaRuudut) g.drawLine(vaaka * ruudunKoko, 0, vaaka * ruudunKoko, pystyRuudut * ruudunKoko)
    for (pysty <- 0 to pystyRuudut) g.drawLine(0, pysty * ruudunKoko, vaakaRuudut * ruudunKoko, pysty * ruudunKoko)
  }
  
  private def piirraMaastot(g: Graphics2D) = { //Piirtää maastoruudut eri väreillä
    val ruudut = pelitilanne.pelilauta.ruudut
    for (vaaka <- 0 until vaakaRuudut; pysty <- 0 until pystyRuudut) {
      ruudut(pysty)(vaaka).maasto match { //Valitaan maaston väri. Kaikki vaihtoehdot käyty läpi!
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
  
  private def piirraAutot(g: Graphics2D) = { //Piirtää autot, tällä hetkellä ympyröillä
    val ruudut = pelitilanne.pelilauta.ruudut
    for (vaaka <- 0 until vaakaRuudut; pysty <- 0 until pystyRuudut) {
      if (ruudut(pysty)(vaaka).onAuto) { //Jos ruudussa on auto 
          if ( ruudut(pysty)(vaaka).auto.get == pelitilanne.pelaajat(0).auto ) g.setColor(Color.BLUE) //Voidaan käyttä get
          else g.setColor(Color.RED)
          g.fillOval(((vaaka + 0.25) * ruudunKoko).toInt , ((pysty + 0.25) * ruudunKoko).toInt, //Piirretään auto
              (0.5*ruudunKoko).toInt, (0.5*ruudunKoko).toInt)
        }
    }
  }
  
  private def piirraVaihtoehdot(g: Graphics2D) = {
    val lauta = pelitilanne.pelilauta
    val autoVuorossa = pelitilanne.vuorossa.auto
    
    if ( autoVuorossa == pelitilanne.pelaajat(0).auto ) g.setColor(Color.BLUE)
    else g.setColor(Color.RED)
    
    for (siirto <- pelitilanne.sallitutSiirrot ) {
      val koordinaatti = siirto.kohdeKoordinaatti
      g.fillRect(((koordinaatti.x + 0.25)*ruudunKoko).toInt, ((koordinaatti.y + 0.25)*ruudunKoko).toInt,
          (0.5*ruudunKoko).toInt, (0.5*ruudunKoko).toInt)
    }
    for (siirto <- pelitilanne.pelilauta.vaihteenSiirrot(autoVuorossa)) { //Käy myös osan samoista kuin edellinen
      val koordinaatti = siirto.kohdeKoordinaatti
      g.fillRect(((koordinaatti.x + 0.375)*ruudunKoko).toInt, ((koordinaatti.y + 0.375)*ruudunKoko).toInt,
          (0.25*ruudunKoko).toInt, (0.25*ruudunKoko).toInt)
    }
  }
  
  listenTo(this.mouse.clicks)
    reactions += {
      case MouseClicked(ruudukko, sijainti, _, _, _) => {
        NappuloidenHallinta.teeSiirto(Koordinaatti(sijainti.x / ruudunKoko, sijainti.y  / ruudunKoko))
        repaint() //Päivitetään heti siirron jälkeen
        if ( pelitilanne.tarkistaVoitto._1.isDefined ) {
          val voittaja = pelitilanne.tarkistaVoitto._1.get
          val tulos = pelitilanne.tarkistaVoitto._2
          val vastaus = Dialog.showConfirmation(null, tulos+"\nPelin voitti: "+voittaja, "Peli päättyi", Dialog.Options.OkCancel, Dialog.Message.Info)
          if (vastaus == Dialog.Result.Ok) {
            NappuloidenHallinta.tallenaPelinTiedot(pelitilanne)// Sulje peli jos ja tallenna tiedot jos OK.
            Ikkuna.top.vaihdaIkkunanSisaltoMenuun()
          }
          else pelitilanne.peruSiirto()
        } else {
          pelitilanne.siirraTekoalya //Siirretään tekoälyä, jos sellainen on pelissä.
          repaint() //Päivitetään uudestaan
        }
        
        //Jos siirto onnistui ja vuoro vaihtui, päivitetään vaihdelaatikon teksti
        NappuloidenHallinta.paivita()
      }
    }
  
  if (pelitilanne.pelaajat(0).onTekoaly) pelitilanne.siirraTekoalya
  
}


package pelikomponentit

import peli.Pelitilanne
import siirrot.Siirto

/* Lähteet:
 * O1-kurssin tehtävän Chess luokka Piece
 */

sealed abstract class Maasto(maastonTunnus: Char) {
  override def toString = maastonTunnus.toString
  def onMaali = this match {
    case Maali(_) => true
    case _ => false
  }
  
  def tyyppi: Maasto = this match {
    case maasto => maasto
  }
}

sealed case class Tie(tietyyppi: Char) extends Maasto(tietyyppi)
case object Reuna extends Maasto(Maasto.reuna)
sealed case class Maali(maalityyppi: Char) extends Maasto(maalityyppi)

object Normaali extends Tie(Maasto.tie)
object Jaa extends Tie(Maasto.jaa)
object Hiekka extends Tie(Maasto.hiekka)

object MaaliYlos extends Maali(Maasto.maaliYlos)
object MaaliAlas extends Maali(Maasto.maaliAlas)
object MaaliOikea extends Maali(Maasto.maaliOikea)
object MaaliVasen extends Maali(Maasto.maaliVasen)
/*
case object Maaliviiva extends Maasto('!')
case object AloitusRuutu1 extends Maasto('1')
case object AloitusRuutu2 extends Maasto('2')
*/

object Maasto {
  val tie = ' '
  val jaa = 'j'
  val hiekka = 'h'
  val reuna = '#'
  val maaliYlos = '^'
  val maaliAlas = 'v'
  val maaliOikea = '>'
  val maaliVasen = '<'
  
  def hiekanSaannot(pelilauta: Pelilauta, siirto: Siirto, auto: Auto) = {
    (!(pelilauta(siirto.lahtoKoordinaatti).onHiekka || pelilauta.lapimentavatRuudut(siirto).exists(_.onHiekka)) || //Jos kulkee hiekalla
      (auto.edellinenSiirto.forall{edellinen =>  edellinen.vaihde >= siirto.vaihde}) ) //vauhti hidastuu
  }
  
  def jaanSaannot(pelilauta: Pelilauta, siirto: Siirto, auto: Auto) = {
    (!(pelilauta(siirto.lahtoKoordinaatti).onJaa || pelilauta.lapimentavatRuudut(siirto).exists(_.onJaa)) || //Jos kulkee jäällä
        (auto.edellinenSiirto.forall{edellinen => //pitää joko nopeuden tai suunnan pysyä muuttumattomana.
           edellinen.vaihde == siirto.vaihde || edellinen.samaSuunta(siirto.vaihde) == siirto.muutaSuunnaksi} ) )
  }

  def apply(maastonTunnus: Char): Maasto = {
    maastonTunnus match {
      case this.reuna => Reuna
      case this.maaliYlos => MaaliYlos
      case this.maaliAlas => MaaliAlas
      case this.maaliOikea => MaaliOikea
      case this.maaliVasen => MaaliVasen
      case this.jaa => Jaa //Tarkoitettujen teiden lisäksi, kaikki tunnistamattomat merkit tulkitaan tieksi.
      case this.hiekka => Hiekka
      case _ => Normaali
    }
  }
}
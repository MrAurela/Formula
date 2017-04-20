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
object SyvaHiekka extends Tie(Maasto.syvaHiekka)
object Oljy extends Tie(Maasto.oljy)

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
  val syvaHiekka = 'H'
  val oljy = 'o'
  val reuna = '#'
  val maaliYlos = '^'
  val maaliAlas = 'v'
  val maaliOikea = '>'
  val maaliVasen = '<'
  
  def hiekanSaannot(pelilauta: Pelilauta, lapimentavatRuudut: Vector[Ruutu], siirto: Siirto, auto: Auto) = {
    !pelilauta(siirto.lahtoKoordinaatti).on(Hiekka) || //Jos lähtee liikkeelle hiekalta
      (auto.edellinenSiirto.forall{edellinen =>  edellinen.vaihde >= siirto.vaihde})  //ei voi nopeuttaa
  }
  
  def syvanHiekanSaannot(pelilauta: Pelilauta, lapimentavatRuudut: Vector[Ruutu], siirto: Siirto, auto: Auto) = {
    !pelilauta(siirto.lahtoKoordinaatti).on(SyvaHiekka) || //Jos lähtee liikkeelle syvällä hiekalla
      (auto.edellinenSiirto.forall{edellinen =>  edellinen.vaihde > siirto.vaihde})  //vauhti hidastuu, kunnes pysähtyy (=häviö)
  }
  
  def jaanSaannot(pelilauta: Pelilauta, lapimentavatRuudut: Vector[Ruutu], siirto: Siirto, auto: Auto) = {
    !( pelilauta(siirto.lahtoKoordinaatti).on(Jaa) || lapimentavatRuudut.exists(_.on(Jaa)) ) || //Jos kulkee jäällä
        (auto.edellinenSiirto.forall{edellinen => //pysyy sama nopeus, samaan suuntaan tai voi ajaa kovempaa minne vaan
           edellinen.muutaSuunnaksi == siirto.muutaSuunnaksi || edellinen.vaihde < siirto.vaihde} )
    
  }
  
  def oljynSaannot(pelilauta: Pelilauta, lapimentavatRuudut: Vector[Ruutu], siirto: Siirto, auto: Auto) = {
    !(pelilauta(siirto.lahtoKoordinaatti).on(Oljy) || lapimentavatRuudut.exists(_.on(Oljy))) || //Jos kulkee öljyssä
        (auto.edellinenSiirto.forall(_.samaSuunta(siirto.vaihde) != siirto.muutaSuunnaksi))
    
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
      case this.syvaHiekka => SyvaHiekka
      case this.oljy => Oljy
      case _ => Normaali
    }
  }
}
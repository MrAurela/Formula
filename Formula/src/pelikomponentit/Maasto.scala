package pelikomponentit

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

case object Tie extends Maasto(Maasto.tie)
case object Reuna extends Maasto(Maasto.reuna)
sealed case class Maali(maalityyppi: Char) extends Maasto(maalityyppi)

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
  val reuna = '#'
  val maaliYlos = '^'
  val maaliAlas = 'v'
  val maaliOikea = '>'
  val maaliVasen = '<'
  
  def apply(maastonTunnus: Char): Maasto = {
    maastonTunnus match {
      case this.reuna => Reuna
      case this.maaliYlos => MaaliYlos
      case this.maaliAlas => MaaliAlas
      case this.maaliOikea => MaaliOikea
      case this.maaliVasen => MaaliVasen
      case _ => Tie //Tarkoitettujen teiden lisäksi, kaikki tunnistamattomat merkit tulkitaan tieksi.
    }
  }
}
package pelikomponentit

/* Lähteet:
 * O1-kurssin tehtävän Chess luokka Piece
 */

sealed abstract class Maasto(maastonTunnus: Char) {
  override def toString = maastonTunnus.toString
}

case object Tie extends Maasto(Maasto.tie)
case object Reuna extends Maasto(Maasto.reuna)
sealed case class Maali(maalityyppi: Char) extends Maasto(maalityyppi)

object Maaliviiva extends Maali(Maasto.maaliviiva)
object AloitusRuutu1 extends Maali(Maasto.aloitusruutu1)
object AloitusRuutu2 extends Maali(Maasto.aloitusruutu2)
/*
case object Maaliviiva extends Maasto('!')
case object AloitusRuutu1 extends Maasto('1')
case object AloitusRuutu2 extends Maasto('2')
*/

object Maasto {
  val tie = ' '
  val reuna = '#'
  val maaliviiva = '!'
  val aloitusruutu1 = '1'
  val aloitusruutu2 = '2'
  
  def apply(maastonTunnus: Char): Maasto = {
    maastonTunnus match {
      case this.reuna => Reuna
      case this.maaliviiva => Maaliviiva
      case this.aloitusruutu1 => AloitusRuutu1
      case this.aloitusruutu2 => AloitusRuutu2
      case _ => Tie //Tarkoitettujen teiden lisäksi, kaikki tunnistamattomat merkit tulkitaan tieksi.
    }
  }
}
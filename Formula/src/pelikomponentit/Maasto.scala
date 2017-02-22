package pelikomponentit

/* Lähteet:
 * O1-kurssin tehtävän Chess luokka Piece
 */


sealed abstract class Maasto(maastonTunnus: Char)

case object Rata extends Maasto(' ')
case object Reuna extends Maasto('#')
sealed case class Maali(maalityyppi: Char) extends Maasto(maalityyppi)

object Maaliviiva extends Maali('!')
object AloitusRuutu1 extends Maali('1')
object AloitusRuutu2 extends Maali('2')
/*
case object Maaliviiva extends Maasto('!')
case object AloitusRuutu1 extends Maasto('1')
case object AloitusRuutu2 extends Maasto('2')
*/
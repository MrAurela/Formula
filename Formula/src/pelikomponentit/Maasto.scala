package pelikomponentit

/* Lähteet:
 * O1-kurssin tehtävän Chess luokka Piece
 */


sealed abstract class Maasto(maastonTunnus: Char)

case object Rata extends Maasto(' ')
case object Reuna extends Maasto('#')
case object Maali extends Maasto('!')
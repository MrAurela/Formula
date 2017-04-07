package tietojenTallennus

import pelikomponentit._

//RADAN NIMI EI VOI SISÄLTÄÄ ÄÄKKÖSIÄ, JOTTA SITÄ VOIDAAN KÄSITELLÄ TIEDOSTOJA LUETTAESSA



class Rata(radanNimi: String, radanMuoto: Vector[Vector[Maasto]]) { //ennatysLista: Vector[Tuple2[String, Int]]
  
  val nimi = radanNimi
  val muoto = radanMuoto
  
  override def toString = nimi + "\n" + radanMuoto.mkString("\n")
}


object Rata {
  
  def apply(nimi: String, tiedot: Vector[String]): Rata = {
    
    //Selvitetään pisimmän vektorin pituus
    val maksimiLeveys = tiedot.map(_.length).fold(0)(Math.max(_,_))
    val radanMaastot = Array.tabulate[Array[Maasto]](tiedot.length)(_=>Array.tabulate(maksimiLeveys)(_=>Tie))
    
    for (j <- 0 until tiedot.length; i <- 0 until tiedot(j).length) {
      radanMaastot(j)(i) = Maasto(tiedot(j)(i))
    }
    
    new Rata(nimi, radanMaastot.map(_.toVector).toVector)
    
  }
  
}
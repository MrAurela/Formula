package tietojenTallennus

import pelikomponentit._

class Rata(nimi: String, radanMuoto: Vector[Vector[Maasto]]) { //ennatysLista: Vector[Tuple2[String, Int]]
  
  val muoto = radanMuoto
}


object Rata {
  
  def apply(nimi: String, tiedot: Vector[String]): Rata = {
    
    //Selvitetään pisimmän vektosin pituus
    val maksimiLeveys = tiedot.map(_.length).fold(0)(Math.max(_,_))
    val radanMaastot = Array.ofDim[Vector[Maasto]](tiedot.length)
    
    for (j <- 0 until tiedot.length) {
      var maastoLista = Array.ofDim[Maasto](tiedot(j).length)
      
      for (i <- 0 until radanMaastot(j).length) {  //Rivin pituuden ajan käydään läpi rivin merkkejä.
        maastoLista(i) = Maasto(tiedot(j)(i))
      }
      for (i <- tiedot(j).length until maksimiLeveys) { //Täydennetään Reuna-maastoa loput, jotta riveistä tulisi yhtä pitkiä.
        maastoLista(i) = Reuna
      }
      
      radanMaastot(j) = maastoLista.toVector
      
    }
    
    new Rata(nimi, radanMaastot.toVector)
    
  }
  
}
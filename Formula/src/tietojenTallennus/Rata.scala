package tietojenTallennus

import pelikomponentit._

class Rata(nimi: String, radanMuoto: Vector[Vector[Maasto]]) { //ennatysLista: Vector[Tuple2[String, Int]]
  
  val muoto = radanMuoto
}


object Rata {
  
  def apply(nimi: String, tiedot: Vector[String]): Rata = {
    
    //Selvitetään pisimmän vektosin pituus
    val maksimiLeveys = tiedot.map(_.length).fold(0)(Math.max(_,_))
    val radanMaastot = Array.tabulate[Array[Maasto]](tiedot.length)(_=>Array.tabulate(maksimiLeveys)(_=>Tie))
    
    /*for (j <- 0 until tiedot.length) {

      var maastoLista = Array.ofDim[Maasto](tiedot(j).length)
      
      for (i <- 0 until maastoLista.length) {  //Rivin pituuden ajan käydään läpi rivin merkkejä.
        maastoLista(i) = Maasto(tiedot(j)(i))
      }
      
      radanMaastot(j) = maastoLista.toVector
      
    }*/
    
    for (j <- 0 until tiedot.length; i <- 0 until tiedot(j).length) {
      radanMaastot(j)(i) = Maasto(tiedot(j)(i))
    }
    
    new Rata(nimi, radanMaastot.map(_.toVector).toVector)
    
  }
  
}
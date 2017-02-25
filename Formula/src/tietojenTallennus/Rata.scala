package tietojenTallennus

import pelikomponentit._

class Rata(nimi: String, radanMuoto: Vector[Vector[Maasto]]) { //ennatysLista: Vector[Tuple2[String, Int]]
  
  val muoto = radanMuoto
}


object Rata {
  
  def apply(nimi: String, tiedot: Vector[String]): Rata = {
    
    //Selvitetään pisimmän vektosin pituus
    val maksimiLeveys = tiedot.map(_.length).fold(0)(Math.max(_,_))
    val radanMaastot = Array.tabulate[Vector[Maasto]](tiedot.length)(_=>Vector.tabulate(maksimiLeveys)(_=>Reuna))
    
    for (j <- 0 until tiedot.length) {

      var maastoLista = Array.ofDim[Maasto](tiedot(j).length)
      
      for (i <- 0 until radanMaastot(j).length) {  //Rivin pituuden ajan käydään läpi rivin merkkejä.
        maastoLista(i) = Maasto(tiedot(j)(i))
      }
      
      radanMaastot(j) = maastoLista.toVector
      
    }
    
    new Rata(nimi, radanMaastot.toVector)
    
    /* Tämä versio luo radan toisin päin, peilikuvana y=-x suhteen
    val radanMaastot = Array.tabulate[Array[Maasto]](maksimiLeveys)(_=>Array.tabulate(tiedot.length)(_=>Reuna))
    
    for (j <- 0 until tiedot.length; i <- 0 until radanMaastot(j).length) {
      radanMaastot(i)(j) = Maasto(tiedot(j)(i))
      
    }
    
    new Rata(nimi, radanMaastot.map(_.toVector).toVector)*/
    
  }
  
}
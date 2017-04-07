package tietojenTallennus

//Tarkistus mapin toiminnasta: http://docs.scala-lang.org/overviews/collections/maps.html

class Profiili(nimi: String) {
  
  var voitetutOttelut = Map[String, Int]() //Radat ja jokaisessa voitetut ottelut
  var pelatutOttelut = Map[String, Int]()  //Radat ja jokaisella pelatut ottelut
  var ennatysajat = Map[String,Int]()      //Radat ja jokaisen ennätysaika
  
  def paivita(voitti: Boolean, rata: String, kierrosaika: Int) = {
    this.pelatutOttelut += rata -> (this.pelatutOttelut.getOrElse(rata, 0) + 1)
    if (voitti) this.voitetutOttelut += rata -> (this.voitetutOttelut.getOrElse(rata, 0) + 1)
    this.ennatysajat += rata -> Math.min( this.ennatysajat.getOrElse(rata, kierrosaika+1), kierrosaika ) //Parempi aika jää voimaan.
    TiedostonHallinta.paivitaProfiili(voitetutOttelut, pelatutOttelut, ennatysajat)
  }
}
package tietojenTallennus

//Tarkistus mapin toiminnasta: http://docs.scala-lang.org/overviews/collections/maps.html
//regex mathcing: http://stackoverflow.com/questions/15119238/scala-regular-expressions-string-delimited-by-double-quotes
import scala.collection.mutable.Map
import peli.Peli


class Profiili(nimi: String) {
  
  var voitetutOttelut = Map[String, Int]() //Radat ja jokaisessa voitetut ottelut
  var pelatutOttelut = Map[String, Int]()  //Radat ja jokaisella pelatut ottelut
  var ennatysajat = Map[String,Int]()      //Radat ja jokaisen ennätysaik
  
  def paivita(voitti: Boolean, rata: String, kierrosaika: Int) = {
    this.pelatutOttelut += rata -> (this.pelatutOttelut.getOrElse(rata, 0) + 1)
    if (voitti) this.voitetutOttelut += rata -> (this.voitetutOttelut.getOrElse(rata, 0) + 1)
    this.ennatysajat += rata -> Math.min( this.ennatysajat.getOrElse(rata, kierrosaika+1), kierrosaika ) //Parempi aika jää voimaan.
    TiedostonHallinta.paivitaProfiili(voitetutOttelut.toMap, pelatutOttelut.toMap, ennatysajat.toMap) //muutetaan immutable.Mapeiksi
  }
  
  override def toString() = nimi +
                            "\nVoitetut ottelut:\n" +
                            voitetutOttelut.toVector.mkString("\n") + 
                            "\nPelatut ottelut:\n" +
                            pelatutOttelut.toVector.mkString("\n") +
                            "\nEnnätysajat:\n" +
                            ennatysajat.toVector.mkString("\n")
                            
  
}

object Profiili {
  
  def apply(nimi: String): Profiili = new Profiili(nimi)
  
  def apply(nimi: String, tiedot: Vector[String]): Profiili = {
    
    val maarittelyRivi = """#([a-z]*)""".r
    val tietoRivi = """([a-zA-Z0-9]+) ([0-9]+)""".r
    val maarittelySanat = Map[String,Map[String,Int]]("voitot" -> Map[String,Int](),
                                                      "pelit" -> Map[String,Int](),
                                                      "ennätykset" -> Map[String,Int]() )
    var luettavaTieto = ""
    
    for ( rivi <- tiedot filter (_.nonEmpty) ) { //Käydään läpi kaikki ei-tyhjät rivit.
      rivi match {
        case maarittelyRivi(tunniste) => {
          if (maarittelySanat.keys.toVector contains tunniste) luettavaTieto = tunniste
          else luettavaTieto = ""
        }
        case tietoRivi(radanNimi, luku) => {
          if (luettavaTieto != "" && Peli.rataLista.map(_.nimi).contains(radanNimi) ) //radan pitää olla olemassa
            maarittelySanat.getOrElse(luettavaTieto, Map[String,Int]()) += (radanNimi -> luku.toInt)
        }
        case _ => {}
      }
      
    }
    
    val uusiProfiili = new Profiili(nimi)
    uusiProfiili.voitetutOttelut = maarittelySanat.getOrElse("voitot", Map[String,Int]())
    uusiProfiili.pelatutOttelut = maarittelySanat.getOrElse("pelit", Map[String,Int]())
    uusiProfiili.ennatysajat = maarittelySanat.getOrElse("ennatykset", Map[String,Int]())
    
    uusiProfiili
  }
  
}
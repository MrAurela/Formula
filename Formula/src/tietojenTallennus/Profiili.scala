package tietojenTallennus

//Tarkistus mapin toiminnasta: http://docs.scala-lang.org/overviews/collections/maps.html
//regex mathcing: http://stackoverflow.com/questions/15119238/scala-regular-expressions-string-delimited-by-double-quotes
// -||- : https://www.tutorialspoint.com/scala/scala_regular_expressions.htm
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import peli.Peli


class Profiili(nimi: String) {
  
  var voitetutOttelut = Map[String, Int]() //Radat ja jokaisessa voitetut ottelut
  var pelatutOttelut = Map[String, Int]()  //Radat ja jokaisella pelatut ottelut
  var ennatysajat = Map[String,Int]()      //Radat ja jokaisen ennätysaika
  
  def paivita(voitti: Boolean, rata: String, kierrosaika: Int) = {
    this.pelatutOttelut += rata -> (this.pelatutOttelut.getOrElse(rata, 0) + 1)
    if (voitti) this.voitetutOttelut += rata -> (this.voitetutOttelut.getOrElse(rata, 0) + 1)
    this.ennatysajat += rata -> Math.min( this.ennatysajat.getOrElse(rata, kierrosaika+1), kierrosaika ) //Parempi aika jää voimaan.
    TiedostonHallinta.paivitaProfiili(nimi, voitetutOttelut.toMap, pelatutOttelut.toMap, ennatysajat.toMap) //Päivitettän myös tiedostoon.
  }
  
  override def toString() = nimi +
                            "\nVoitetut ottelut:\n" +
                            voitetutOttelut.mkString("\n") +
                            "\nPelatut ottelut:\n" +
                            pelatutOttelut.mkString("\n") + 
                            "\nEnnätysajat:\n" +
                            ennatysajat.mkString("\n")
}

object Profiili {
  
  //Luo uuden profiilin, jolla ei ole pelitietoja.
  def apply(nimi: String): Profiili = new Profiili(nimi)
  
  //Tulkitsee tiedostosta ladatut rivit Profiiliksi
  //Hyväksyy tiedot vain muodossa: radanNimi voitotRadalla kaikkiPelitRadalla ennatysaikaRadalla.
  def apply(nimi: String, tiedot: Vector[String]): Profiili = {
    
    val riviMalli = """([a-zA-Z0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)""".r
    val radat = Peli.rataLista
    val voitot = Map[String,Int]()
    val pelit = Map[String,Int]()
    val ennatykset = Map[String,Int]()
    
    var virheellisetRivit = Buffer[String]() //Ottaa talteen virheelliset rivit. EI TÄLLÄ HETKELLÄ KÄYTETÄ MIHINKÄÄN.
    
    for ( rivi <- tiedot) { //Käydään läpi kaikki rivit
      rivi match {
        case riviMalli(radanNimi, voitotRadalla, pelitRadalla, ennatys)
         if ( radat.map(_.nimi).contains(radanNimi) && voitotRadalla.toInt <= pelitRadalla.toInt)  => {
          voitot += radanNimi -> voitotRadalla.toInt
          pelit += radanNimi -> pelitRadalla.toInt
          ennatykset += radanNimi -> ennatys.toInt
        }
        case _ => virheellisetRivit.append(rivi)
      }
    }
    
    /* VANHA IDEA TIEDOSTON LUKEMISEEN
     * 
     * 
     * val maarittelyRivi = """#([a-z]*)""".r
    	 val tietoRivi = """([a-zA-Z0-9]+) ([0-9]+)""".r
    	 
    	 val maarittelySanat = Map[String,Map[String,Int]]("voitot" -> Map[String,Int](),
                                                      "pelit" -> Map[String,Int](),
                                                      "ennätykset" -> Map[String,Int]() )
       var luettavaTieto = ""
     * 
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
      
    }*/
    
    val uusiProfiili = new Profiili(nimi)
    uusiProfiili.voitetutOttelut = voitot
    uusiProfiili.pelatutOttelut = pelit
    uusiProfiili.ennatysajat = ennatykset
    
    uusiProfiili
  }
  
}
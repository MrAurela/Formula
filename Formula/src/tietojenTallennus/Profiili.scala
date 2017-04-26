package tietojenTallennus

//Lähteet:
//Tarkistus mapin toiminnasta: http://docs.scala-lang.org/overviews/collections/maps.html
//regex mathcing: http://stackoverflow.com/questions/15119238/scala-regular-expressions-string-delimited-by-double-quotes
// -||- : https://www.tutorialspoint.com/scala/scala_regular_expressions.html

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import peli.Peli


class Profiili(_nimi: String) {
  
  override def toString() = nimi
  
  val nimi = this._nimi
  var voitetutOttelut = Map[String, Int]() //Radat ja jokaisessa voitetut ottelut
  var pelatutOttelut = Map[String, Int]()  //Radat ja jokaisella pelatut ottelut
  var ennatysajat = Map[String,Option[Int]]() //Radat ja jokaisen mahdollinen ennätysaika
  
  def paivita(voitti: Boolean, rata: String, optionKierrosaika: Option[Int]) = {
    this.pelatutOttelut += rata -> (this.pelatutOttelut.getOrElse(rata, 0) + 1)
    if (voitti) this.voitetutOttelut += rata -> (this.voitetutOttelut.getOrElse(rata, 0) + 1)
    else this.voitetutOttelut += rata -> 0
    this.ennatysajat += (rata -> minOption(this.ennatysajat.getOrElse(rata,None), optionKierrosaika)) //Parempi aika jää voimaan. Määrittämätön aika on aina huonompi.
    TiedostonHallinta.paivitaProfiili(nimi, voitetutOttelut.toMap, pelatutOttelut.toMap, ennatysajat.toMap) //Päivitettän myös tiedostoon.
    
    def minOption(o1: Option[Int], o2: Option[Int]): Option[Int] = {
      if (o1.isEmpty && o2.isEmpty) None
      else if (o1.isEmpty) o2
      else if (o2.isEmpty) o1
      else if (o1.get > o2.get) o2 //Get voidaan käyttää, sillä kumpikaan ei ole tyhjä jos suoritus jatkuu tänne.
      else o1
    }
  }
  
  //Radat, joista tallessa täysi informaatio
  def radat = 
    voitetutOttelut.keys.toVector.filter{rata => pelatutOttelut.keys.toVector.contains(rata)}
  
  private def ratatiedotTekstiksi(rata: String) = {
    rata + ": " + voitetutOttelut.getOrElse(rata, "-") + ", " +
                  pelatutOttelut.getOrElse(rata, "-") + ", " +
                  ennatysajat.getOrElse(rata, Some("-")).getOrElse("-") //Jos joko ei määritelty ennatysajassa tai määritelty Noneksi -> "-"
  }
  
  def tiedot = radat.map(this.ratatiedotTekstiksi(_))
                            
}

object Profiili {
  
  //Luo uuden profiilin, jolla ei ole pelitietoja.
  def apply(nimi: String): Profiili = new Profiili(nimi)
  
  //Tulkitsee tiedostosta ladatut rivit Profiiliksi
  //Hyväksyy tiedot vain muodossa: radanNimi voitotRadalla kaikkiPelitRadalla ennatysaikaRadalla.
  def apply(nimi: String, tiedot: Vector[String]): Profiili = {
    
    val riviMalli = """([a-zA-Z0-9]+) ([0-9]+) ([0-9]+) ([0-9-]+)""".r
    val radat = Peli.rataLista
    val voitot = Map[String,Int]()
    val pelit = Map[String,Int]()
    val ennatykset = Map[String,Option[Int]]()
    
    var virheellisetRivit = Buffer[String]() //Ottaa talteen virheelliset rivit. EI TÄLLÄ HETKELLÄ KÄYTETÄ MIHINKÄÄN.
    
    for ( rivi <- tiedot) { //Käydään läpi kaikki rivit
      rivi match {
        case riviMalli(radanNimi, voitotRadalla, pelitRadalla, ennatys)
         if ( radat.map(_.nimi).contains(radanNimi) && voitotRadalla.toInt <= pelitRadalla.toInt)  => {
          voitot += radanNimi -> voitotRadalla.toInt
          pelit += radanNimi -> pelitRadalla.toInt
          if (ennatys=="-") ennatykset += radanNimi -> None //Jos ennätystä ei ole määritelty 
          else ennatykset += radanNimi -> Some(ennatys.toInt)
        }
        case _ => virheellisetRivit.append(rivi)
      }
    }
    
    val uusiProfiili = new Profiili(nimi)
    uusiProfiili.voitetutOttelut = voitot
    uusiProfiili.pelatutOttelut = pelit
    uusiProfiili.ennatysajat = ennatykset
    
    uusiProfiili
  }
  
}
package tietojenTallennus

import java.io._
import scala.collection.mutable.Buffer
import pelikomponentit.Maasto

/* Lähteet:
 * A+ -materiaali, erityisesti sivu: https://plus.cs.hut.fi/studio_2/2017/k15/osa03/
 * Tiedostojen lukeminen ja hallinta, sekä listFiles()-komento
 * Vastaus ongelmaan map-komennon kanssa sivulta http://www.scala-lang.org/old/node/12090.html
 */


//KYSYMYS: lueRadanRivit; voiko return-rivi olla ennne finallya? Käydäänkö finally kuitenkin?

object TiedostonHallinta {
  
  private val rataKansio = "radat"
  private val ratojenEnnatyslistat = "radat/enntatykset"
  
  def haeRadat: Vector[Rata] = {
    val rataTiedostot = new File(rataKansio).listFiles().map(_.getName).toVector //Haetaan kaikkien kansion tiedostojen nimet.
    val ratojenMuodot = rataTiedostot.map(this.lueRata(_)).toVector //Haetaan ratojen sisällöt samassa järjestyksessä.
    val tiedot = rataTiedostot.zip(ratojenMuodot).filter(_._2.isDefined) //Poistetaan yhdistelmät, joissa rata on None.
    val valmis = tiedot.unzip._1.zip(tiedot.unzip._2.map(_.get)) //Poistetaaan Some-kääreet
    valmis.map(monikko => Rata(monikko._1, monikko._2)) //Luodaan lopullinen ratalista
  }
  
  private def lueRata(rataTiedosto: String): Option[Vector[String]] = {
    try {
      val tiedosto = new FileReader(rataTiedosto) //Yritetään avata tiedosto
      val riviTiedosto = new BufferedReader(tiedosto)
      this.lueRivit(tiedosto, riviTiedosto, rataTiedosto)
    } catch {
      case virheIlmoitus: FileNotFoundException => { //Tätä ei pitäisi tapahtua, jos lueRata, kutsutaan tarkoituksenmukaisesti.
        println(virheIlmoitus)
        println("Tiedostoa "+rataTiedosto+" ei löytynyt kansiosta "+rataKansio+".")
        None
      }
      case virheIlmoitus : Throwable => { //Hallitsee kaikki muut virheet paitsi tiedoston puuttumisen.
        println(virheIlmoitus)
        println("Odottamaton virhe tapahtui yrittäessa lukea tiedostoa "+rataTiedosto+".")
        None
      }
    }
  }
  
  private def lueRivit(tiedosto: FileReader, rivit: BufferedReader, tiedostonNimi: String): Option[Vector[String]] = {
    try {
      val riviLista = Buffer[String]()
      var rivi = rivit.readLine()
      while (rivi != null) {
        riviLista.append(rivi)
        rivi = rivit.readLine()
      }
      Some(riviLista.toVector)
    } catch {
      case virheIlmoitus: Throwable => { //Hallitsee kaikki virheet, joita voi tapahtua tiedoston lukemisen aikan.
        println(virheIlmoitus)
        println("Odottamaton virhe tapahtui yrittäessä lukea tiedoston "+tiedostonNimi+" rivejä.")
        None
      }
    } finally {
      tiedosto.close()
      rivit.close()
    }
  }
    
  
  
}
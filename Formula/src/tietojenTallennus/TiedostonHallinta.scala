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
//Miksei try-catch toimi haeRadassa? --> NullPointerException

object TiedostonHallinta {
  
  private val rataKansio = "radat"
  private val ratojenEnnatyslistat = "radat/ennatykset"
  private val profiiliKansio = "profiilit"
  
  def haeRadat: Vector[Rata] = {
    try {
      val kansio = new File(rataKansio)
      val rataTiedostot = kansio.listFiles().map(kansio.getName+"/"+_.getName).toVector //Haetaan kaikkien kansion tiedostojen nimet.
      val ratojenMuodot = rataTiedostot.map(this.lueTiedosto(_, rataKansio)).toVector //Haetaan ratojen sisällöt samassa järjestyksessä.
      val tiedot = rataTiedostot.zip(ratojenMuodot).filter(_._2.isDefined) //Poistetaan yhdistelmät, joissa rata on None.
      val valmis = tiedot.unzip._1.zip(tiedot.unzip._2.map(_.get)) //Poistetaaan Some-kääreet
      valmis.map(monikko => Rata(monikko._1, monikko._2)) //Luodaan lopullinen ratalista
    } catch {
      case virheIlmoitus: FileNotFoundException => { //Tänne päädytäään jos kansio puuttuu.
        println(virheIlmoitus)
        println("Kansiota "+rataKansio+" ei löytynyt.")
        Vector()
      }
      case virheIlmoitus : NullPointerException => { //Hallitsee nullpointer exceptionin.
        println(virheIlmoitus)
        println("NullPointerException tapahtui yrittäessa lukea tiedostoa "+rataKansio+".")
        Vector()
      }
      case virheIlmoitus : Throwable => { //Hallitsee kaikki muut virheet paitsi kansion puuttumisen.
        println(virheIlmoitus)
        println("Odottamaton virhe tapahtui yrittäessa avata kansiota "+rataKansio+".")
        Vector()
      }
    }
  }
  
  //Kansio josta luetaan määrä sen, ohjataanko tiedostonjen käsittely lueRatatiedosto vai lueProfiilitiedosto -funktiolle.
  private def lueTiedosto(tiedostonNimi: String, luettavaKansio: String): Option[Vector[String]] = {
    try {
      val tiedosto = new FileReader(tiedostonNimi) //Yritetään avata tiedosto
      val riviTiedosto = new BufferedReader(tiedosto)
      if (luettavaKansio == rataKansio) this.lueRataTiedosto(tiedosto, riviTiedosto, tiedostonNimi) //Luetaan tiedosto ratasyntaksilla
      else if (luettavaKansio == profiiliKansio) this.lueProfiiliTiedosto(tiedosto, riviTiedosto, tiedostonNimi) // profiilisyntaksilla
      else None
    } catch {
      case virheIlmoitus: FileNotFoundException => { //Tätä ei pitäisi tapahtua, jos lueRata, kutsutaan tarkoituksenmukaisesti.
        println(virheIlmoitus)
        println("Tiedostoa "+tiedostonNimi+" ei löytynyt kansiosta "+luettavaKansio+".")
        None
      }
      case virheIlmoitus : Throwable => { //Hallitsee kaikki muut virheet paitsi tiedoston puuttumisen.
        println(virheIlmoitus)
        println("Odottamaton virhe tapahtui yrittäessa lukea tiedostoa "+tiedostonNimi+".")
        None
      }
    }
  }
  
  private def lueRataTiedosto(tiedosto: FileReader, rivit: BufferedReader, tiedostonNimi: String): Option[Vector[String]] = {
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
  
  def haeProfiilit(): Vector[Profiili] = {
    val kansio = new File(profiiliKansio)
    val profiiliTiedostot = kansio.listFiles().map(kansio.getName+"/"+_.getName).toVector //Haetaan kaikkien kansion tiedostojen nimet.
    ???
  }
  
  private def lueProfiiliTiedosto(tiedosto: FileReader, rivit: BufferedReader, tiedostonNimi: String) = {
    ???
  }
  
  def paivitaProfiili(voitot: Map[String,Int], kaikkiPelit: Map[String,Int], ennatykset: Map[String,Int]) = {
    ???
  }
  
  def uusiProfiili(nimi: String) = {
    val profiili = new Profiili(nimi)
    ???
  }
    
  
  
}
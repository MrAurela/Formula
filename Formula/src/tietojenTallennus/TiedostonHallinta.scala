package tietojenTallennus

import java.io._
import scala.collection.mutable.Buffer
import pelikomponentit.Maasto

/* Lähteet:
 * A+ -materiaali, erityisesti sivu: https://plus.cs.hut.fi/studio_2/2017/k15/osa03/
 * Tiedostojen lukeminen ja hallinta, sekä listFiles()-komento
 * Vastaus ongelmaan map-komennon kanssa sivulta http://www.scala-lang.org/old/node/12090.html
 * Tiedostojen kirjoittaminen: http://alvinalexander.com/scala/how-to-write-text-files-in-scala-printwriter-filewriter
 */

object TiedostonHallinta {
  
  private val rataKansio = "radat"
  private val profiiliKansio = "profiilit"
  
  def haeRadat: Vector[Rata] = {
    this.haeTiedostoista(rataKansio).map(monikko => Rata(monikko._1, monikko._2)) //Luodaan ratalista
  }
  
  def haeProfiilit: Vector[Profiili] = {
    this.haeTiedostoista(profiiliKansio).map(monikko => Profiili(monikko._1, monikko._2)) // Luodaan profiililista
  }
  
  private def haeTiedostoista(kansionNimi: String): Vector[(String,Vector[String])] = {
    try {
      val kansio = new File(kansionNimi)
      val kansionTiedostot = kansio.listFiles().toVector //Haetaan kaikki kansion tiedostot.
      val tiedostojenNimet = kansionTiedostot.map(_.getName) //Otetaan talteen tiedostojen nimet.
      val ratojenNimet = tiedostojenNimet.map(_.takeWhile(_!='.')) //Radan/profiilin nimi saadaan suoraan tiedoston nimestä.
      val tiedostojenKokoNimet = tiedostojenNimet.map(kansionNimi+"/"+_).toVector // -> kansio/tiedostonNimi.txt
      
      val ratojenMuodot = tiedostojenKokoNimet.map(this.lueTiedosto(_)).toVector //Haetaan tiedostojen rivit samassa järjestyksessä.
      val tiedot = ratojenNimet.zip(ratojenMuodot).filter(_._2.isDefined) //Yhdistetään ratojen/profiilien nimet ja hyväksytyt tiedot (!= None)
      val valmis = tiedot.unzip._1.zip(tiedot.unzip._2.map(_.get)) //Poistetaaan Some-kääreet
      
      valmis // Palautetaan radan/profiilin nimet yhdistettynä niiden sisältöön.
    } catch {
      case virheIlmoitus: FileNotFoundException => { //Tänne päädytäään jos kansio puuttuu.
        println(virheIlmoitus)
        println("Kansiota "+kansionNimi+" ei löytynyt.")
        Vector()
      }
      case virheIlmoitus : NullPointerException => { //Hallitsee nullpointer exceptionin.
        println(virheIlmoitus)
        println("NullPointerException tapahtui yrittäessa lukea tiedostoa "+kansionNimi+".")
        Vector()
      }
      case virheIlmoitus : Throwable => { //Hallitsee kaikki muut virheet paitsi kansion puuttumisen.
        println(virheIlmoitus)
        println("Odottamaton virhe tapahtui yrittäessa avata kansiota "+kansionNimi+".")
        Vector()
      }
    }
  }
  
  //Kansio josta luetaan määrä sen, ohjataanko tiedostonjen käsittely lueRatatiedosto vai lueProfiilitiedosto -funktiolle.
  private def lueTiedosto(tiedostonNimi: String): Option[Vector[String]] = {
    try {
      val tiedosto = new FileReader(tiedostonNimi) //Yritetään avata tiedosto
      val riviTiedosto = new BufferedReader(tiedosto)
      this.lueRivit(tiedosto, riviTiedosto, tiedostonNimi)
    } catch {
      case virheIlmoitus: FileNotFoundException => { //Tätä ei pitäisi tapahtua, jos lueRata, kutsutaan tarkoituksenmukaisesti.
        println(virheIlmoitus)
        println("Tiedostoa "+tiedostonNimi+" ei löytynyt.")
        None
      }
      case virheIlmoitus : Throwable => { //Hallitsee kaikki muut virheet paitsi tiedoston puuttumisen.
        println(virheIlmoitus)
        println("Odottamaton virhe tapahtui yrittäessa lukea tiedostoa "+tiedostonNimi+".")
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
  
  def paivitaProfiili(nimi: String, voitot: Map[String,Int], kaikkiPelit: Map[String,Int], ennatykset: Map[String,Option[Int]]) = {
    var teksti = ""
    for (rata <- kaikkiPelit.keys) {
      if (voitot.keys.toVector.contains(rata)) { //Radan tiedot täytyy löytyä aina kahdesta ensimmäisestä
        teksti += rata + " " + voitot.getOrElse(rata, "-") + " " +
                  kaikkiPelit.getOrElse(rata, "-") + " " + ennatykset.getOrElse(rata, Some("-")).getOrElse("-") + "\n" //Muutetaan haluttuun muotoon.
      }
    }
    val tiedosto = profiiliKansio + "/" + nimi + ".txt"
    this.kirjoitaTiedostoon(tiedosto, teksti)
  }
  
  def uusiProfiili(profiili: Profiili) = {
    this.paivitaProfiili(profiili.nimi, profiili.voitetutOttelut.toMap, profiili.pelatutOttelut.toMap, profiili.ennatysajat.toMap)
  }
  
  def paivitaRata(nimi: String, radanMuoto: Vector[String], ennatykset: Vector[(String, Int)]) = {
    var teksti = ""
    radanMuoto.foreach(teksti += _+"\n") //Ensin radan muotoa kuvaavat tiedot.
    ennatykset.foreach{pari: (String, Int) => teksti += pari._1 + " " + pari._2 + "\n"} //Sitten ennätyslista
    
    val tiedosto = rataKansio + "/" + nimi + ".txt"
    this.kirjoitaTiedostoon(tiedosto, teksti)
  }
  
  def uusiRata(rata: Rata) = {
    this.paivitaRata(rata.nimi, rata.muotoTekstina, rata.ennatysKierrokset.toVector)
  }
  
  private def kirjoitaTiedostoon(tiedostonNimi: String, sisalto: String) = {
    try { //Yritetään avata tiedosto
      val tiedosto = new File(tiedostonNimi)
      val  kirjoittaja = new BufferedWriter( new FileWriter(tiedosto) )
      try { //Yritetään kirjoittaa tiedostoon. 
        kirjoittaja.write(sisalto)
      } catch {
        case virheIlmoitus : Throwable => { //Jos mikä tahansa virhe tiedoston kirjoittamisen aikana.
          println(virheIlmoitus)
          println("Tiedostoon " + tiedostonNimi + " ei pystytty kirjoittamaan.")
        }
      } finally { //Joka tapauksessa suljetaan tiedosto.
        kirjoittaja.close()
      }
    } catch {
      case virheIlmoitus: IOException => { //Jos IOEXception tapahtuu.
        println(virheIlmoitus)
        println("Tiedostoon " + tiedostonNimi + " ei pystytty avaamaan.")
      }
      case virheIlmoitus: Throwable => { //Jos joku muu virhe tapahtuu.
        println(virheIlmoitus)
        println("Tiedostoa " + tiedostonNimi + " avattaessa tapahtui odottamaton virhe.")
      }
    }
  }
    
  
  
}
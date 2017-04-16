package tietojenTallennus

import pelikomponentit._
import scala.collection.mutable.Buffer

//RADAN NIMI EI VOI SISÄLTÄÄ ÄÄKKÖSIÄ, JOTTA SITÄ VOIDAAN KÄSITELLÄ TIEDOSTOJA LUETTAESSA



class Rata(radanNimi: String, radanMuoto: Vector[Vector[Maasto]], ennatykset: Map[String, Int]) {
  
  val nimi = radanNimi
  val muoto = radanMuoto
 
  var ennatysKierrokset = ennatykset //Mapin ansioista kaikilla ajajilla on tallessa vain paras aika.
  
  def parhaatAjajat = ennatykset.toVector.sortWith(_._2 < _._2)
  
  override def toString = nimi + "\n" + radanMuoto.mkString("\n")
  
  def paivita(kierrosajat: Map[String, Option[Int]]) = {
    for (tulos <- kierrosajat) {
      if (tulos._2.isDefined) ennatysKierrokset += tulos._1 -> tulos._2.get //Lisätään vain jos kirrosaika määritetty (pelaaja pääsi maaliin).
    }
    val muotoTekstina = muoto.map(_.map(_.toString).mkString(""))
    TiedostonHallinta.paivitaRata(nimi, muotoTekstina, ennatysKierrokset.toVector)
  }
}


///ONGELMA RATOJEN LATAUKSESSA: Vector(Vector(), Vector((hei,1)))

object Rata {
  
  def apply(nimi: String, tiedot: Vector[String]): Rata = {
    
    var ennatykset = Map[String, Int]()
    var radanMuoto = Buffer[String]()
    
    //Jaetaan tiedot radan muotoon ja nopeinten kierrosten listaan.
    val ennatysMalli = """([a-zA-Z0-9]+) ([0-9])+""".r
    var i = 0
    
    for (rivi <- tiedot) {
      rivi match {
        case ennatysMalli(nimi, kierrosaika) => ennatykset += (nimi -> kierrosaika.toInt) //Käsitellään ennätykset heti
        case _ => radanMuoto.append(rivi) //Tallennetaan ratamuotoiset rivit ja luodaan lopuksi itse rata.
      }
    }
    
    //Radan muoto.
    //Selvitetään pisimmän alkion pituus
    val maksimiLeveys = radanMuoto.map(_.length).fold(0)(Math.max(_,_))
    val radanMaastot = Array.tabulate[Array[Maasto]](radanMuoto.length)(_=>Array.tabulate(maksimiLeveys)(_=>Tie))
    
    for (j <- 0 until radanMuoto.length; i <- 0 until radanMuoto(j).length) {
      radanMaastot(j)(i) = Maasto(radanMuoto(j)(i))
    }
    
    new Rata(nimi, radanMaastot.map(_.toVector).toVector, ennatykset)
    
  }
  
}
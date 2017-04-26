package tietojenTallennus

import pelikomponentit._
import scala.collection.mutable.Buffer

class Rata(radanNimi: String, radanMuoto: Array[Array[Maasto]], ennatykset: Map[String, Int]) {
  override def toString = nimi + "\n" + radanMuoto.mkString("\n")
  
  val nimi = radanNimi
  val muoto = radanMuoto
  def muotoTekstina = muoto.toVector.map(_.toVector.map(_.toString).mkString(""))
 
  require(muoto.forall(_.length == muoto(0).length)) //Taulukon pitää olla suorakaide.
  require(muoto.length > 0)
  
  val korkeus = muoto.length
  val leveys = muoto(0).length
  
  var ennatysKierrokset = ennatykset //Mapin ansioista kaikilla ajajilla on tallessa vain paras aika.
  
  def parhaatAjajat = ennatysKierrokset.toVector.sortWith(_._1 < _._1).sortWith(_._2 < _._2) //Ensisijaisesti tuloksen, sitten nimen mukaan.
  
  def parhaatAjajatTeksti = parhaatAjajat.map{pari => pari._1 +  " " + pari._2}
  
  def paivita(kierrosajat: Map[String, Option[Int]]) = {
    for (tulos <- kierrosajat) {
      if (tulos._2.isDefined) ennatysKierrokset += tulos._1 -> tulos._2.get //Lisätään vain jos kirrosaika määritetty (pelaaja pääsi maaliin).
    }
    TiedostonHallinta.paivitaRata(nimi, muotoTekstina, ennatysKierrokset.toVector)
  }
  
  def muutaMaasto(y: Int, x: Int, maasto: Maasto) = {
    muoto(y)(x) = maasto
  }
  
  def onkoEhja: Boolean = this.muoto.flatten.toList.count(_.onMaali) >= 2
  
}

object Rata {
  
  def uusi(nimi: String, leveys: Int, korkeus: Int) = {
    new Rata(nimi,  Array.tabulate(korkeus, leveys)((_,_)=>Maasto(Maasto.tie)), Map[String, Int]())
  }
  
  def kopio(nimi: String, rata: Rata) = {
    new Rata(nimi, Array.tabulate(rata.korkeus, rata.leveys)((y,x)=>rata.muoto(y)(x)), Map[String, Int]())
  }
  
  def apply(nimi: String, tiedot: Vector[String]): Rata = {
    
    var ennatykset = Map[String, Int]()
    var radanMuoto = Buffer[String]()
    
    //Jaetaan tiedot radan muotoon ja nopeinten kierrosten listaan.
    val ennatysMalli = """([a-zA-Z0-9]+) ([0-9]+)""".r
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
    val radanMaastot = Array.tabulate[Array[Maasto]](radanMuoto.length)(_=>Array.tabulate(maksimiLeveys)(_=>Normaali))
    
    for (j <- 0 until radanMuoto.length; i <- 0 until radanMuoto(j).length) {
      radanMaastot(j)(i) = Maasto(radanMuoto(j)(i))
    }
    
    new Rata(nimi, radanMaastot, ennatykset)
    
  }
  
}
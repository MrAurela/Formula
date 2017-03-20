package siirrot

import Math._
import scala.collection.mutable.Buffer

class Siirto(lahto: Koordinaatti, kohde: Koordinaatti) {
  
  val lahtoKoordinaatti = lahto
  val kohdeKoordinaatti = kohde
  
  val xLiike: Int = kohde.x - lahto.x
  val yLiike: Int = kohde.y - lahto.y
  val liike: Double = sqrt(pow(xLiike,2)+pow(yLiike,2))
  
  val vaihde = max(Math.abs(xLiike), Math.abs(yLiike)) //Liike on aina vähintään toiseen suuntaan tasan vaihteen mittainen
  val kulma: Double = if (xLiike > 0 && yLiike <= 0) asin(abs( yLiike / liike )) //Kulma radiaaneissa 0-2PI
                      else if (xLiike <= 0 && yLiike < 0) PI - asin(abs( yLiike / liike ))
                      else if (xLiike < 0 && yLiike >= 0) PI + asin(abs( yLiike / liike ))
                      else 2*PI - asin(abs( yLiike / liike ))
  val kulmaAsteina: Double = (this.kulma*360)/(2*Math.PI)
  
  //KÄYTETÄÄNKÖ MIHINKÄÄN?
  val kulmaKerroin: Option[Double] = {
    if (xLiike != 0 ) Some( yLiike.toDouble / xLiike.toDouble )
    else None
  }
  
  def muutaSuunnaksi = Suunta( Koordinaatti(this.xLiike, this.yLiike) ) // Siirtovektori siirrettynä alkamaan origosta.
  
  def samaSuunta(vaihde: Int): Suunta = {
    println(vaihde)
    if (vaihde < 1 || vaihde > 5) Suunta(Koordinaatti(0,0)) //Virheellisellä vaihteella saadaan 0-suunta.
    else if (vaihde == this.vaihde) this.muutaSuunnaksi     //Jos vaihde on sama, jatketaan täsmälleen samaan suuntaan.
    else Suunta(vaihde, this.kulma)                         //Luo Suunnan annetulla vaihteella mahdollisimman lähellä
                                                            //annettua kulmaa.
  }
  
  //VOISI EHKÄ YKSINKERTAISTAA YHDISTÄMÄLLÄ EHTOJA
  def myotapaivaNaapuri: Siirto = {
    if (xLiike == yLiike) { // Vino liike (oikealle alas tai vasemmalle ylös)
      new Siirto( this.lahto, Koordinaatti(this.kohde.x-signum(yLiike).toInt, this.kohde.y) )
    } else if (xLiike == -yLiike) { // Vino liike (oikealle ylös tai vasemmalle alas
      new Siirto( this.lahto, Koordinaatti(this.kohde.x, this.kohde.y+signum(xLiike).toInt) )
    } else if (abs(xLiike) > abs(yLiike)) { //Enemmän vaakasuuntainen liike
      new Siirto( this.lahto, Koordinaatti(this.kohde.x, this.kohde.y+signum(xLiike).toInt) )
    } else { // Enemmän pystysuuntainen liike
      new Siirto( this.lahto, Koordinaatti(this.kohde.x-signum(yLiike).toInt, this.kohde.y) )
    } 
  }
  
  def vastapaivaNaapuri: Siirto = {
    if (xLiike == yLiike) { // Vino liike (oikealle alas tai vasemmalle ylös)
      new Siirto( this.lahto, Koordinaatti(this.kohde.x, this.kohde.y-signum(xLiike).toInt ) )
    } else if (xLiike == -yLiike) { // Vino liike (oikealle ylös tai vasemmalle alas
      new Siirto( this.lahto, Koordinaatti(this.kohde.x+signum(yLiike).toInt, this.kohde.y) )
    } else if (abs(xLiike) > abs(yLiike)) { //Enemmän vaakasuuntainen liike
      new Siirto( this.lahto, Koordinaatti(this.kohde.x, this.kohde.y-signum(xLiike).toInt) )
    } else { // Enemmän pystysuuntainen liike
      new Siirto( this.lahto, Koordinaatti(this.kohde.x+signum(yLiike).toInt, this.kohde.y) )
    } 
  }
  
  /* Laskee koordinaatit, joiden läpi kulkee suora, joka lähtee auton lähtöruudun keskipisteestä
   * ja päättyy auton kohderuudun keskipisteeseen.
   */
  def lapimentavatKoordinaatit: Vector[Koordinaatti] = {
    
    val koordinaatit = Buffer[Koordinaatti]()
    def lisaaKoordinaattiJosUusi(koordinaatti: Koordinaatti) { //Apufunktio listaan lisäämiselle
      if ( !koordinaatit.contains(koordinaatti) ) {
        koordinaatit.append(koordinaatti)
        println(koordinaatti+" lisättiin.")
      } else println(koordinaatti+" ei lisätty.")
    }
    
    var leveaSuunta = 0 //Alustetaan muuttujat
    var kapeaSuunta = 0
    var posSuunta = 1 // Kuljetaanko x/y -suuntaa positiiviseen vai negatiivisesn suuntaan, 1 tai -1
    var xSuunta = false
    var vinoSuora = false //45 asteen kulmassa kulkeva suora on erikoistapaus, joka tarkastellaan erikseen.
    
    if (abs(this.xLiike) >= abs(this.yLiike)) {
      leveaSuunta = this.xLiike
      kapeaSuunta = this.yLiike
      xSuunta = true
      if (this.xLiike > 0) posSuunta = 1
      else posSuunta = -1
    } else if (abs(this.xLiike) < abs(this.yLiike)){
      leveaSuunta = this.yLiike
      kapeaSuunta = this.xLiike
      xSuunta = false //Tämä vain selkeyden vuoksi tässä
      if (this.yLiike > 0) posSuunta = 1
      else posSuunta = -1
    } else {
      vinoSuora = true
      println("POIKKEUSTAPAUS: EI VIELÄ TOTEUTETTU")
    }

    val kulmakerroin: Double = kapeaSuunta.toDouble / leveaSuunta.toDouble
    def suoranFunktio(muuttuja: Double): (Int, Boolean) = {
      println("Funktio: "+ (kulmakerroin * muuttuja - 0.5*(kulmakerroin-1) ) )
      var tulos = kulmakerroin * muuttuja - 0.5*(kulmakerroin-1)
      if (tulos < 0) tulos -= 1 //vähennetään yksi, jotta -0.4 pyöristettäisiin -1:teen (-0.4-1 = -1.4 = -1)
      val onkoTasan = tulos.toInt == tulos
      (tulos.toInt, onkoTasan)
    }
    
    println(this.lahtoKoordinaatti+" => "+this.kohdeKoordinaatti)
    //println(kulmakerroin)


    if (xSuunta) {
      var levea = posSuunta
      while (levea != leveaSuunta+posSuunta) { //Käydään jokainen alkava ruutu läpi
        println(levea)
        val funktionArvo = suoranFunktio(levea)._1
        val onkoTasan = suoranFunktio(levea)._2
        if ( !onkoTasan ) //Jos funktion arvo on tasaluku, ollaan menty kulman läpi. Tällöin ensimmäisen läpi ei kuljeta.
          lisaaKoordinaattiJosUusi( this.lahtoKoordinaatti + Koordinaatti(levea-posSuunta, funktionArvo) )
        lisaaKoordinaattiJosUusi( this.lahtoKoordinaatti + Koordinaatti(levea, funktionArvo) )
        levea += posSuunta //Edetään tiettyyn suuntaan (koko ajan eteen tai taakse)
      }
    } else {
      var levea = posSuunta
      while (levea != leveaSuunta+posSuunta) { //Käydään jokainen alkava ruutu läpi
        println(levea)
        val funktionArvo = suoranFunktio(levea)._1
        val onkoTasan = suoranFunktio(levea)._2
        if ( !onkoTasan ) //Jos funktion arvo on tasaluku, ollaan menty kulman läpi. Tällöin ensimmäisen läpi ei kuljeta.
          lisaaKoordinaattiJosUusi( this.lahtoKoordinaatti + Koordinaatti(funktionArvo, levea-posSuunta) )
        lisaaKoordinaattiJosUusi( this.lahtoKoordinaatti + Koordinaatti(funktionArvo, levea ) )
        levea += posSuunta //Edetään tiettyyn suuntaan (koko ajan eteen tai taakse)
      }
    }
    

   
    koordinaatit.toVector
    /* aiempi yritys
    var edellinenY = this.lahtoKoordinaatti.y
    for (x <- this.lahtoKoordinaatti.x to this.kohdeKoordinaatti.x) {
      for ( y <- edellinenY to funktio(x) ) {
        println(x+", "+y)
        koordinaatit.append( Koordinaatti(x,y) )
      }
      edellinenY = funktio(x)
    }
    
    koordinaatit.toVector
    */
  }
  
  override def toString() = this.lahto.toString() + " => " + this.kohde.toString()
}
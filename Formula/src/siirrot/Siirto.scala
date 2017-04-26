package siirrot

import Math._
import scala.collection.mutable.Buffer

//Lähteet:
//Läpimentävien koordinaattien laskeminen pohjautui: https://fi.wikipedia.org/wiki/Bresenhamin_algoritmi

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
  
  def muutaSuunnaksi = Suunta( Koordinaatti(this.xLiike, this.yLiike) ) // Siirtovektori siirrettynä alkamaan origosta.
  
  def samaSuunta(vaihde: Int): Suunta = {
    if (vaihde < 1 || vaihde > 5) Suunta(Koordinaatti(0,0)) //Virheellisellä vaihteella saadaan 0-suunta.
    else if (vaihde == this.vaihde) this.muutaSuunnaksi     //Jos vaihde on sama, jatketaan täsmälleen samaan suuntaan.
    else Suunta(vaihde, this.kulma)                         //Luo Suunnan annetulla vaihteella mahdollisimman lähellä
                                                            //annettua kulmaa.
  }
  
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
  
  def lapimentavatKoordinaatit: Vector[Koordinaatti] = {
    
    var koordinaatit = Buffer[Koordinaatti]()
    
    val leveys = this.xLiike.toDouble
    val korkeus = this.yLiike.toDouble
    
    //Hallitaan erikoistapaus, jossa siirron kulmakerronta laskettaessa pitäisi jakaa nollalla.
    if ( leveys == 0 && korkeus == 0) koordinaatit = Buffer[Koordinaatti]() //Virheellinen siirto
    else if ( leveys == 0 ) { 
      val lahtoY = this.lahtoKoordinaatti.y
      val lahtoX = this.lahtoKoordinaatti.x
      for ( y <- lahtoY to lahtoY + korkeus.toInt by signum(korkeus).toInt) koordinaatit.append(Koordinaatti(lahtoX, y) )
    } 
    
    //Kaikki muut tapaukset käsitellään laskemalla siirron kulmakerroin ja käymällä sitten suoraa läpi
    //siirron alkupisteestä sen loppupisteeseen.
    else{ 

      val kulmakerroin: Double = korkeus / leveys
      
      //Apufunktio, joka laskee suoran y-koordinaatin x-koordinaatin perusteella.
      def suoranFunktio(muuttuja: Double): Double = kulmakerroin * muuttuja - 0.5*(kulmakerroin-1)
      
      //Apufunktio koordinaattien lisäämiselle.
      //Muuttaa desimaaliluvut kokonaisluvuiksi ja lisää koordinaatin vain jos se on uusi.
      def lisaaKoordinaatti(xDouble: Double, yDouble: Double) = { 
        var xInt = if (xDouble < 0) (xDouble - 1).toInt //vähennetään yksi, jotta -0.4 pyöristettäisiin -1:teen (-0.4-1 = -1.4 = -1)
                else xDouble.toInt
        var yInt = if (yDouble < 0) (yDouble - 1).toInt
                else yDouble.toInt                                            //Laskussa siirto alkaa aina koordinaatista (0,0)
        val koordinaatti = Koordinaatti(xInt, yInt) + this.lahtoKoordinaatti  //Vaihdetaan alkamaan lahtoKoordinaatista koordinaattien(vektorien) summalla.
        if ( !koordinaatit.contains(koordinaatti) ) {                        
          koordinaatit.append(koordinaatti)
        }
      }
      
      //Edetään suoraa pitkin ja lisätään listaan kaikki ruudut, joissa käydään.
      for ( x <- 0.5 to leveys+0.5 by Math.signum(leveys)*0.01 ) {
        lisaaKoordinaatti( x, suoranFunktio(x) )
      }
      
    }
    
    if (koordinaatit.size > 1) koordinaatit = koordinaatit.tail //Korjaa "empty.tail" bugin
    koordinaatit.toVector
    
  }
  
  override def toString() = this.lahto.toString() + " => " + this.kohde.toString()
}
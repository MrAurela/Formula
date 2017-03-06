package siirrot

import Math._

class Siirto(lahto: Koordinaatti, kohde: Koordinaatti) {
  
  val lahtoKoordinaatti = lahto
  val kohdeKoordinaatti = kohde
  
  val xLiike: Int = kohde.x - lahto.x
  val yLiike: Int = kohde.y - lahto.y
  val liike: Double = sqrt(pow(xLiike,2)+pow(yLiike,2))
  
  val vaihde = max(xLiike, yLiike) //Liike on aina vähintään toiseen suuntaan tasan vaihteen mittainen
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
  
  override def toString() = this.lahto.toString() + " => " + this.kohde.toString()
}
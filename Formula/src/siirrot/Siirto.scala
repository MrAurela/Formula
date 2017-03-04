package siirrot

import Math._

class Siirto(lahto: Koordinaatti, kohde: Koordinaatti) {
  
  val xLiike = kohde.x - lahto.x
  val yLiike = kohde.y - lahto.y
  
  val vaihde = max(xLiike, yLiike) //Liike on aina vähintään toiseen suuntaan tasan vaihteen mittainen
  val kulma: Option[Double] = if (xLiike == 0) None
                              else Some ( atan( yLiike.toDouble / xLiike ) )
  
  
  def muutaSuunnaksi = Suunta( Koordinaatti(this.xLiike, this.yLiike) ) // Siirtovektori siirrettynä alkamaan origosta.
  
  def samaSuunta(vaihde: Int): Suunta = {
    if (vaihde < 1 || vaihde > 5) Suunta(Koordinaatti(0,0)) //Virheellisellä vaihteella saadaan 0-suunta.
    else if (vaihde == this.vaihde) this.muutaSuunnaksi     //Jos vaihde on sama, jatketaan täsmälleen samaan suuntaan.
    else {
      
    }
    ???
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
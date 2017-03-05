package siirrot

import scala.collection.mutable.Buffer

case class Suunta(kohde: Koordinaatti) extends Siirto(Koordinaatti(0,0), kohde) {
  
  def muutaSiirroksi(lahto: Koordinaatti) = new Siirto(lahto, this.kohde)
  
  override def toString() = this.kohde.toString()
}

object Suunta {
  
  //Luo Suunnan, joka on tiettyä vaihdetta ja mahdollisimman lähellä annettua kulmaa
  def apply(vaihde: Int, kulma: Double): Suunta = {
    val vaihtoehdot = suunnatVaihteella(vaihde)
    Suunta(kulma, vaihtoehdot)
  }
  
  //Valitsee annetuista Suunnista sen, joka on lähimpänä annettua kulmaa
  def apply(kulma: Double, vaihtoehdot: Vector[Suunta]): Suunta = {
    if (vaihtoehdot.length > 0) {
      vaihtoehdot.reduce{(suunta1, suunta2) =>
        if (Math.abs(suunta1.kulma - kulma) <= Math.abs(suunta2.kulma - kulma)) suunta1
        else suunta2
      }
    } else {
      Suunta(Koordinaatti(0,0)) //Virhelliseen syötteeseen vastataan 0-suunnalla
    }
  }
  
  //Listaa kaikki mahdolliset suunnaat, joihin tietyllä vaihteella pääsee.
  def suunnatVaihteella(vaihde: Int): Vector[Suunta] = {
    val suunnat = Buffer[Suunta]()
    for (i <- -vaihde to vaihde; j <- -vaihde to vaihde) { //Pisin edettävä suunta on vaihteen arvo
      if ( Math.max( Math.abs(i), Math.abs(j) ) == vaihde ) suunnat.append(Suunta(Koordinaatti(i,j)))
    }
    suunnat.toVector
  }

}
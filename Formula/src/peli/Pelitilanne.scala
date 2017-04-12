
package peli

import pelikomponentit.Pelilauta
import siirrot.{Koordinaatti, Siirto}
import  pelikomponentit._

class Pelitilanne(lauta: Pelilauta, pelaajaLista: Vector[Pelaaja]) {
  
  val pelaajat = pelaajaLista
  val pelilauta = lauta
  
  var vuorossa = pelaajaLista(0)
  val vaadittavatKierrokset = 1
  var peliKaynnissa = true
 
  def eiVuorossa = if (this.vuorossa == pelaajat(0)) pelaajat(1) else pelaajat(0)
  
  def siirraAutoa(kohde: Koordinaatti) = {
    val siirtoOnnistui = this.pelilauta.siirraAutoaLaillisesti(vuorossa.auto, kohde) //Siirtää autoa, jos siirto on laillinen
    if (siirtoOnnistui) uusiVuoro()
  }
  
  def peruSiirto() = {
    val auto = this.eiVuorossa.auto
    if (auto.edellinenSiirto.isDefined) {
      val edellinenKoordinaatti = auto.edellinenSiirto.get.lahtoKoordinaatti //get voidaan käyttää, koska se Option on määritelty
      pelilauta.siirraAutoaPakolla(auto, edellinenKoordinaatti, false) //Siirretään auto takaisin, ei muisteta siirtoa
      auto.poistaEdellinenSiirto() //Poistetaan edellinenkin siirto listasta
      auto.palautaEdellinenVaihde()
      this.vaihdaVuoroa() //Perutun auton vuoro
    }
  }

  def sallitutSiirrot = pelilauta.sallitutSiirrot(vuorossa.auto)
  
  private def uusiVuoro() = {
    if (vuorossa.auto.kierrokset >= vaadittavatKierrokset) peliKaynnissa = false
    else vaihdaVuoroa()
  }
  
  private def vaihdaVuoroa() = {
    if (pelaajat(0) == vuorossa) vuorossa = pelaajat(1)
    else vuorossa = pelaajat(0)
    vuorossa.auto.aloitaVuoro()
  }
  
}

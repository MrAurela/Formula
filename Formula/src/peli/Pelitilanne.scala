
package peli

import pelikomponentit.Pelilauta
import siirrot.{Koordinaatti, Siirto}
import  pelikomponentit._

class Pelitilanne(lauta: Pelilauta, pelaajaLista: Vector[Pelaaja]) {
  
  val pelaajat = pelaajaLista
  val pelilauta = lauta
  
  var vuorossa = pelaajaLista(0)
  val vaadittavatKierrokset = 1
 
  def eiVuorossa = if (this.vuorossa == pelaajat(0)) pelaajat(1) else pelaajat(0)
  
  def siirraAutoa(kohde: Koordinaatti) = {
    val siirtoOnnistui = this.pelilauta.siirraAutoaLaillisesti(vuorossa.auto, kohde) //Siirtää autoa, jos siirto on laillinen
    if (siirtoOnnistui) vaihdaVuoroa()
  }
  
  def peruSiirto() = {
    val auto = this.eiVuorossa.auto
    if (auto.edellinenSiirto.isDefined) {
      val edellinenKoordinaatti = auto.edellinenSiirto.get.lahtoKoordinaatti //get voidaan käyttää, koska se Option on määritelty
      val nykyinenKoordinaatti = auto.edellinenSiirto.get.kohdeKoordinaatti
      //Jos auto siirtyi maaliin edellisellä siirrolla, sen vaikutus pitää perua kun auto siirtyy takaisin.
      if (pelilauta.ruudut(nykyinenKoordinaatti.y)(nykyinenKoordinaatti.x).onMaali) auto.lisaaKierros(-1)
      pelilauta.siirraAutoaPakolla(auto, edellinenKoordinaatti, false) //Siirretään auto takaisin, ei muisteta siirtoa
      auto.poistaEdellinenSiirto() //Poistetaan edellinenkin siirto listasta
      auto.palautaEdellinenVaihde()
      this.vaihdaVuoroa() //Perutun auton vuoro
    }
  }

  def sallitutSiirrot = pelilauta.sallitutSiirrot(vuorossa.auto, true) //True => tietyllä vaihteella sallitut siirrot
  
  def kaikkiSallitutSiirrot = pelilauta.sallitutSiirrot(vuorossa.auto, false) //False => sallittujen vaihteiden kaikki siirrot
  
  //ALASPÄIN SEINÄÄN TÖRMÄYS EI TOIMI, SININEN, RATA2
  def tarkistaVoitto: (Option[Pelaaja], String) =  {
    if (pelaajat(0).auto.tehdytSiirrot == pelaajat(1).auto.tehdytSiirrot) { //Maaliin pääsemisesssä annetaan "tasoittava vuoro"
      if (pelaajat(1).auto.kierrokset >= vaadittavatKierrokset) (Some(pelaajat(1)), "Rata kierretty.") //Tasatilanteessa voittaa toisena siirtävä pelaaja.
      else if (pelaajat(0).auto.kierrokset >= vaadittavatKierrokset) (Some(pelaajat(0)), "Rata kierretty.")
      else (None, "")
    }
    
    if (pelaajat(0).auto.eiVoiLiikkua(this)) (Some(pelaajat(1)), "Ulosajo.") //Jos ei voi liikkua laillisesti, häviää
    else if (pelaajat(1).auto.eiVoiLiikkua(this)) (Some(pelaajat(0)), "Ulosajo.")
    else (None, "")

  }
  
  def onkoMaalissa(pelaaja: Pelaaja): Boolean = pelaaja.auto.kierrokset >= vaadittavatKierrokset
  
  private def vaihdaVuoroa() = {
    if (pelaajat(0) == vuorossa) vuorossa = pelaajat(1)
    else vuorossa = pelaajat(0)
    vuorossa.auto.aloitaVuoro()
  }
  
}

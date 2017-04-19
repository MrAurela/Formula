
package peli

import pelikomponentit.Pelilauta
import siirrot.{Koordinaatti, Siirto}
import tietojenTallennus.Profiili
import  pelikomponentit._

class Pelitilanne(lauta: Pelilauta, pelaajaLista: Vector[Pelaaja]) {
  
  override def toString = pelaajaLista.map(_.toString).mkString("\n") + "\n" + pelilauta.etsiAuto(pelaajat(0).auto) + "\n" +
                          pelilauta.etsiAuto(pelaajat(1).auto)
  
  val pelaajat = pelaajaLista
  val pelilauta = lauta
  
  var vuorossa = pelaajaLista(0)
  val vaadittavatKierrokset = 1
 
  def eiVuorossa = if (this.vuorossa == pelaajat(0)) pelaajat(1) else pelaajat(0)
  
  def siirraAutoa(kohde: Koordinaatti): Unit = {
    val siirtoOnnistui = this.pelilauta.siirraAutoaLaillisesti(vuorossa.auto, kohde) //Siirtää autoa, jos siirto on laillinen
    if (siirtoOnnistui) vaihdaVuoroa()
  }
  
  def peruSiirto(): Unit = {
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
  
  def kaikkiSallitutSiirrot(auto: Auto) = pelilauta.sallitutSiirrot(auto, false) //False => sallittujen vaihteiden kaikki siirrot
  
  def tarkistaVoitto: (Option[Pelaaja], String) =  {
    println("Tarkistetaan voittoa. KONE: "+pelaajat(0).auto.kierrokset+", PELAAJA: "+pelaajat(1).auto.kierrokset)
    println("KONE: "+pelaajat(0).auto.siirrot.size+", PELAAJA: "+pelaajat(1).auto.siirrot.size)
    if (pelaajat(0).auto.tehdytSiirrot == pelaajat(1).auto.tehdytSiirrot) { //Maaliin pääsemisesssä annetaan "tasoittava vuoro"
      if (this.onkoMaalissa(pelaajat(1)) && this.onkoMaalissa(pelaajat(0))) return (Some(pelaajat(1)), "Molemmat pelaajat kiersivät radan.")
      else if (this.onkoMaalissa(pelaajat(1))) return (Some(pelaajat(1)), "Punainen kiersi radan.") //Tasatilanteessa voittaa toisena siirtävä pelaaja.
      else if (this.onkoMaalissa(pelaajat(0))) return (Some(pelaajat(0)), "Sininen kiersi radan.")
    }
    if (vuorossa.auto.eiVoiLiikkua(this))
        if (vuorossa==pelaajat(0)) (Some(pelaajat(1)),"Sininen ajaa ulos.") //Jos ei voi liikkua laillisesti, häviää
        else (Some(pelaajat(0)), "Punainen ajaa ulos.")
    else (None, "")
  }
  
  def onkoMaalissa(pelaaja: Pelaaja): Boolean = {
    pelaaja.auto.kierrokset >= vaadittavatKierrokset
  }
  
  def siirraTekoalya = {
    if (vuorossa.onTekoaly) this.siirraAutoa( vuorossa.tekoaly.get.siirto ) //Tekoäly siirtää autoa vuorollaan.
  }
  
  private def vaihdaVuoroa() = {
    if (pelaajat(0) == vuorossa) vuorossa = pelaajat(1)
    else vuorossa = pelaajat(0)
    vuorossa.auto.aloitaVuoro()
  }
  
}

object Pelitilanne {
  
  def kuvitteellinen(ai: AI): Pelitilanne = {
    val uusi = new Pelilauta(ai.pelitilanne.pelilauta.rata)
    val vanhaAuto1 = ai.pelitilanne.pelaajat(0).auto
    val vanhaAuto2 = ai.pelitilanne.pelaajat(1).auto
    val auto1 = new Auto
    val auto2 = new Auto
    val autot = Vector(auto1, auto2)
    val lahtoAuto1 = if (vanhaAuto1.siirrot.isDefinedAt(0)) vanhaAuto1.siirrot(0).lahtoKoordinaatti //Etsitään uusille autoille lähdöt
                     else ai.pelitilanne.pelilauta.etsiAuto( vanhaAuto1 )
    val lahtoAuto2 = if (vanhaAuto2.siirrot.isDefinedAt(0)) vanhaAuto2.siirrot(0).lahtoKoordinaatti
                     else ai.pelitilanne.pelilauta.etsiAuto( vanhaAuto2 )
    uusi.ruudut(lahtoAuto1.y)(lahtoAuto1.x).lisaaAuto(auto1) //Asetetaan autot uuden laudan lähtöihin.
    uusi.ruudut(lahtoAuto2.y)(lahtoAuto2.x).lisaaAuto(auto2)
    auto1.asetaAloitusSuunta(ai.pelitilanne.pelilauta.ruudut(lahtoAuto1.y)(lahtoAuto1.x).maasto) //Asetetaan autojen lähtösuunnat
    auto2.asetaAloitusSuunta(ai.pelitilanne.pelilauta.ruudut(lahtoAuto2.y)(lahtoAuto2.x).maasto)
    vanhaAuto1.siirrot.foreach{siirto => uusi.siirraAutoaLaillisesti(auto1, siirto.kohdeKoordinaatti)} //Toistetaan autojen aiemmat siirrot
    vanhaAuto2.siirrot.foreach{siirto => uusi.siirraAutoaLaillisesti(auto2, siirto.kohdeKoordinaatti)} //uudella laudalla

    val profiili1 = if (ai.pelitilanne.pelaajat(0).onTekoaly) Some(new Profiili(Peli.ai.nimi)) else None //Luodaan uudet profiilit
    val profiili2 = if (!ai.pelitilanne.pelaajat(0).onTekoaly) Some(new Profiili(Peli.ai.nimi)) else None //toinen AI, toinen None
    val pelaaja1 = new Pelaaja(profiili1, auto1) //Luodaan uudet pelaajat 
    val pelaaja2 = new Pelaaja(profiili2, auto2)
    val pelaajat = Vector(pelaaja1, pelaaja2)
    pelaajat.foreach{pelaaja => if (pelaaja.profiili.isDefined) pelaaja.asetaAI(ai)} //Asetetaan AI:lle AI
    
    val kuviteltuTilanne = new Pelitilanne(uusi, pelaajat)
    kuviteltuTilanne.vuorossa = if (pelaaja1.onTekoaly) pelaaja1 else pelaaja2 //Asetetaan tietokonepelaaja vuoroon.
    kuviteltuTilanne
  }
  
}

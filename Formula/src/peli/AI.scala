package peli

import siirrot._
import java.lang.StackOverflowError
import pelikomponentit.Auto

class AI(pelitilanne_ : Pelitilanne) {
  
  var pelitilanne = pelitilanne_
  
  //TEKOÄLY VALITSEE AINA ENSIMMÄISEN SIIRTOVAIHTOEHDON? MIKSEI LÖYDÄ PAREMPAA
  
  def siirto: Koordinaatti = {
    var kuviteltuTilanne = Pelitilanne.kuvitteellinen(this)
    val itse = kuviteltuTilanne.vuorossa
    val sijainti = kuviteltuTilanne.pelilauta.etsiAuto(itse.auto)
    
    def etsiParasSiirtoSarja(pelitilanne: Pelitilanne, siirrot: List[Siirto]): (List[Siirto], Double) = {
      try {
        val vuorossa = pelitilanne.vuorossa
        println(pelitilanne.kaikkiSallitutSiirrot(vuorossa.auto))
        if (vuorossa == itse) println(pelitilanne.pelilauta.etsiAuto(itse.auto))
        
        pelitilanne.kaikkiSallitutSiirrot(vuorossa.auto).reverse.foreach{ siirto: Siirto => 
          pelitilanne.siirraAutoa(siirto.kohdeKoordinaatti)
          val arvostelu = arvosteleTilanne(pelitilanne, siirrot) //Tarkistetaan onko jompikumpi voittanut
          if (arvostelu == 1.0 && vuorossa == itse) {
            //("Pisteet: "+arvostelu)
            return (siirrot ++ List(siirto), arvostelu) //Jos löydetään voitto, tyydytään siihen.
          } else if (arvostelu == -1.0 && vuorossa != itse) {
            //println("Pisteet: "+arvostelu)
            return (siirrot ++ List(siirto), arvostelu) //Jos vastustaja löytää voiton, hän tyytyy siihen.
          } else if (Math.abs(arvostelu) != 1.0) //Jos siirto ei ole varmasti huono, etsitään syvemmältä
              etsiParasSiirtoSarja(pelitilanne, siirrot ++ List(siirto))
          pelitilanne.peruSiirto() //Otetaan äskeinen siirto takaisin.
        }
        
        //Jos tullaan tänne, yhtään voittoa ei löytynyt.
        (siirrot, -1.0)
        
      } catch {
        case e: StackOverflowError => (siirrot, 0)
      }
    
    }
    //Palautetaan 1.0 jos tietokone on voittanut aseman ja -1.0 jos pelaaja on voittanut aseman. 0 jos peli on kesken.
    def arvosteleTilanne(pelitilanne: Pelitilanne, siirrot: List[Siirto]): Double = {
      val mahdollinenVoittaja = pelitilanne.tarkistaVoitto._1
      //println("Arvostellaan tilanne: "+pelitilanne.tarkistaVoitto._2)
      if (mahdollinenVoittaja.isDefined) {
        if (mahdollinenVoittaja.get == itse) 1.0
        else {
          -1.0
        }
      }
      else if (itse.auto.kierrokset <= -1) -1.0 //Tekoäly ei halua miinuskierroksille
      else 0.0
    }
    
    val siirrotJaArvo = etsiParasSiirtoSarja(kuviteltuTilanne, List[Siirto]())
    
    if (siirrotJaArvo._1.size > 0) {
      //println("VALITTIIN "+siirrotJaArvo)
      siirrotJaArvo._1(0).kohdeKoordinaatti
    } else {
      pelitilanne.sallitutSiirrot(0).kohdeKoordinaatti //Jos paremapaa ei löydetty, otetaan ensimmäinen siirto
    }
    //itse.auto.sallitutSuunnat(1).muutaSiirroksi(sijainti).kohdeKoordinaatti
  }
  /*
  //Hakee eri vaihteilla kaikki ruudut, joista voi siirtyä maaliin.
  def voittavatSiirrot = {
    val lauta = pelitilanne.pelilauta
    for (sarake <- 0 until lauta.leveys; rivi <- 0 until lauta.korkeus) {
      val ruutu = lauta.ruudut(rivi)(sarake)
      if (ruutu.voiAjaa) { //Jos ruutu on sallittu sijainti
        val auto = new Auto
        ruutu.lisaaAuto(auto)
        
      }
    }
  }*/
  
  
}
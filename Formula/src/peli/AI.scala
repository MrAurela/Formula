package peli

import siirrot._
import java.lang.StackOverflowError
import pelikomponentit.Auto
import scala.collection.mutable.Buffer

//Lähteet:
// min-funktion Ordering: http://www.scala-lang.org/old/node/7529

class AI(pelitilanne_ : Pelitilanne) {
  
  var pelitilanne = pelitilanne_
  
  def siirto(): Koordinaatti = {
    var kuviteltuTilanne = Pelitilanne.kuvitteellinen(this)
    val itse = kuviteltuTilanne.vuorossa
    val sijainti = kuviteltuTilanne.pelilauta.etsiAuto(itse.auto)
    
    def etsiParasSiirtoSarja(pelitilanne: Pelitilanne, siirrot: List[Siirto]): (List[Siirto], Int) = {
      try {
        if (siirrot.size <= 5 ) {
          val vuorossa = pelitilanne.vuorossa
          
          var vaihtoehdot = Buffer[(List[Siirto],Int)]()
          
          pelitilanne.kaikkiSallitutSiirrot(vuorossa.auto).foreach{ siirto: Siirto =>
            if (siirto.vaihde > vuorossa.auto.vaihde) vuorossa.auto.nostaVaihdetta() //Vaihdetaan siirrolle sopiva vaihde
            else if (siirto.vaihde < vuorossa.auto.vaihde) vuorossa.auto.laskeVaihdetta()
            
            pelitilanne.siirraAutoa(siirto.kohdeKoordinaatti) //siirretään autoa
            
            val arvostelu = arvosteleTilanne(pelitilanne, siirrot) //Tarkistetaan onko jompikumpi voittanut

            if (arvostelu != 0) vaihtoehdot.append( (siirrot ++ List(siirto), arvostelu) ) //Jos on, palautetaan siirrot ja arvo
            else {
              val v = etsiParasSiirtoSarja(pelitilanne, siirrot ++ List(siirto)) //Muutoin etsitään paras jatko ja palautetaan se
              vaihtoehdot.append( v )
            }
            
            pelitilanne.peruSiirto() //Otetaan äskeinen siirto takaisin.
          }
          
          //Palautetaan AI:n kannalta paras siirto, jos on AI:n vuoro ja vastustajan kannalta paras siirto (AI:lle huonoin),
          //jos on vastustajan vuoro
          if (vuorossa == itse) valitseParas(vaihtoehdot)
          else valitseHuonoin(vaihtoehdot)
      
      } else {
        (siirrot, 0) //Jos laskua jatketaan liian pitkälle, reitin arvo on 0
      }
        
      } catch {
        case e: StackOverflowError => {
          (siirrot, 0) //Jos laskua ei jakseta laskea loppuun, reitin arvo on 0
        }
        case _: Throwable => (siirrot, 0) //Samoin jos jokin muu virhe sattuu.
      }
    
    }
    
    //Jos pelaaja on voittamassa, palautetaan positiivinen luku, kuinka monta kierrosta voittoon menee
    //Jos pelaaja on häviämässä, palautetaan negatiivinen luku, kuinka monta kierrosta häviämiseen menee
    //Muutoin 0.
    def arvosteleTilanne(pelitilanne: Pelitilanne, siirrot: List[Siirto]): Int = {
      val mahdollinenVoittaja = pelitilanne.tarkistaVoitto._1
      if (mahdollinenVoittaja.isDefined) {
        if (mahdollinenVoittaja.get == itse) siirrot.size
        else -siirrot.size
      }
      else 0
    }
    
    def valitseParas(lista: Buffer[(List[Siirto],Int)]): (List[Siirto],Int) = {
      require(lista.size > 0)
      val positiiviset = lista.filter(_._2 > 0)
      val neutraalit = lista.filter(_._2 == 0)
      val negatiiviset = lista.filter(_._2 < 0)
      if (positiiviset.size > 0) positiiviset.min(Ordering.by{pari: (List[Siirto],Int) => pari._2}) //Pienin siirtomäärä on paras voitto
      else if (neutraalit.size > 0) neutraalit.max(Ordering.by{pari: (List[Siirto],Int) => pari._1(0).vaihde})
      else negatiiviset.min(Ordering.by{pari: (List[Siirto],Int) => pari._2}) //Itseisarvoiltaan suurin -> pisin häviö, on paras.
    }
    
    def valitseHuonoin(lista: Buffer[(List[Siirto],Int)]): (List[Siirto],Int) = {
      require(lista.size > 0)
      val positiiviset = lista.filter(_._2 > 0)
      val neutraalit = lista.filter(_._2 == 0)
      val negatiiviset = lista.filter(_._2 < 0)
      if (negatiiviset.size > 0) negatiiviset.max(Ordering.by{pari: (List[Siirto],Int) => pari._2}) //Nopein häviö on huonoin
      else if (neutraalit.size > 0) neutraalit.max(Ordering.by{pari: (List[Siirto],Int) => pari._1(0).vaihde})
      else positiiviset.max(Ordering.by{pari: (List[Siirto],Int) => pari._2}) //Hitain voitto on huonoin.
    }

    val parasSiirtoSarja = etsiParasSiirtoSarja(kuviteltuTilanne, List[Siirto]())
    val siirto = parasSiirtoSarja._1(0)
    
    if (siirto.vaihde > pelitilanne.vuorossa.auto.vaihde) pelitilanne.vuorossa.auto.nostaVaihdetta() //Vaihdetaan siirrolle sopiva vaihde
    else if (siirto.vaihde < pelitilanne.vuorossa.auto.vaihde) pelitilanne.vuorossa.auto.laskeVaihdetta()
    siirto.kohdeKoordinaatti //Palautetaan kohderuutu

  }
  
}
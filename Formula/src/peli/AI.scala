package peli

import siirrot._

class AI(pelitilanne_ : Pelitilanne) {
  
  var pelitilanne = pelitilanne_
  
  //TUTKI TOIMIIKO KUVITELTU TILANNE SITTEN JATKA TÄTÄ
  
  def siirto: Koordinaatti = {
    var kuviteltuTilanne = Pelitilanne.kuvitteellinen(this)
    val itse = kuviteltuTilanne.vuorossa
    val sijainti = kuviteltuTilanne.pelilauta.etsiAuto(itse.auto)
    
    //TESTATTIIN KUVITELMAA
    //kuviteltuTilanne.pelilauta.siirraAutoaPakolla(itse.auto, Koordinaatti(3,7), false)
    //println(kuviteltuTilanne.toString)
    
    def etsiParasSiirtoSarja(pelitilanne: Pelitilanne, vuorossa: Pelaaja, siirrot: List[Siirto]): (List[Siirto], Double) = {
      println("Vuorossa: "+vuorossa)
      println("Koordinaatissa: "+pelitilanne.pelilauta.etsiAuto(vuorossa.auto))
      val vaihtoehdot = pelitilanne.kaikkiSallitutSiirrot(vuorossa.auto).map{ siirto: Siirto => 
        println(siirto)
        pelitilanne.pelilauta.siirraAutoaLaillisesti(vuorossa.auto, siirto.kohdeKoordinaatti)
        //println("Siirtyi ruutuun: "+pelitilanne.pelilauta.etsiAuto(vuorossa.auto))
        val tulos = 
          if (Math.abs(arvosteleTilanne(pelitilanne, siirrot)) == 1.0) {
            (siirrot ++ List(siirto), arvosteleTilanne(pelitilanne,siirrot))
          }
          else etsiParasSiirtoSarja(pelitilanne,
                                    if (vuorossa==pelitilanne.pelaajat(0)) pelitilanne.pelaajat(1)
                                    else pelitilanne.pelaajat(0),
                                    siirrot ++ List(siirto))
        tulos
      }
      
      val valinta =
        if (vaihtoehdot.size > 0) 
          if (vuorossa == itse) vaihtoehdot.maxBy[Double]{pari => pari._2} else vaihtoehdot.maxBy[Double]{pari => -pari._2}
        else
          (siirrot, -1.0)
      println(valinta)
      valinta
    }

    def arvosteleTilanne(pelitilanne: Pelitilanne, siirrot: List[Siirto]): Double = {
      val mahdollinenVoittaja = pelitilanne.tarkistaVoitto._1
      if (mahdollinenVoittaja.isDefined) {
        if (mahdollinenVoittaja.get == itse) 1.0
        else -1.0
      } else if (siirrot.size >= 100) -1.0
      else {
        0.0
      }
    }
    
    val siirrotJaArvo = etsiParasSiirtoSarja(kuviteltuTilanne, itse, List[Siirto]())
    println("VALITTIIN "+siirrotJaArvo)
    siirrotJaArvo._1(0).kohdeKoordinaatti
    //itse.auto.sallitutSuunnat(1).muutaSiirroksi(sijainti).kohdeKoordinaatti
    

  }
  
}
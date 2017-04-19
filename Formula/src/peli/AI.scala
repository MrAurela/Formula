package peli

import siirrot._

class AI(pelitilanne_ : Pelitilanne) {
  
  var pelitilanne = pelitilanne_
  
  //TUTKI TOIMIIKO KUVITELTU TILANNE SITTEN JATKA TÄTÄ
  
  def siirto: Koordinaatti = {
    var kuviteltuTilanne = Pelitilanne.kuvitteellinen(this)
    val itse = kuviteltuTilanne.vuorossa
    val sijainti = kuviteltuTilanne.pelilauta.etsiAuto(itse.auto)
    
    def etsiParasSiirtoSarja(pelitilanne: Pelitilanne, siirrot: List[Siirto]): (List[Siirto], Double) = {
      val vuorossa = pelitilanne.vuorossa
      
      if (vuorossa == itse) println(pelitilanne.pelilauta.etsiAuto(itse.auto))
      
      val vaihtoehdot = pelitilanne.kaikkiSallitutSiirrot(vuorossa.auto).map{ siirto: Siirto => 
        pelitilanne.pelilauta.siirraAutoaLaillisesti(vuorossa.auto, siirto.kohdeKoordinaatti) //Siirretään autoa
        vaihdaVuoroa() //Vaihdetaan vuoroa
        val arvostelu = arvosteleTilanne(pelitilanne, siirrot) //Tarkistetaan onko jompokumpo voittanut
        val tulos =
          if (Math.abs(arvostelu) == 1.0) {
            println("Pisteet: "+arvostelu)
            (siirrot ++ List(siirto), arvostelu)
          }
          else etsiParasSiirtoSarja(pelitilanne, siirrot ++ List(siirto))
        pelitilanne.pelilauta.siirraAutoaPakolla(vuorossa.auto, siirto.lahtoKoordinaatti, false) //Otetaan siirto takaisin.
        tulos
      }
      
      //println(vaihtoehdot)
      
      val valinta =
        if (vaihtoehdot.size > 0)
          if (vuorossa == itse) vaihtoehdot.maxBy[Double]{pari => pari._2} else vaihtoehdot.maxBy[Double]{pari => -pari._2}
        else {
          println("Vaihtoehdot on tyhjä! "+pelitilanne.kaikkiSallitutSiirrot(vuorossa.auto).mkString(", "))
          (siirrot, -1.0)}
      valinta
    
    }
    //Palautetaan 1.0 jos tietokone on voittanut aseman ja -1.0 jos pelaaja on voittanut aseman. 0 jos peli on kesken.
    def arvosteleTilanne(pelitilanne: Pelitilanne, siirrot: List[Siirto]): Double = {
      val mahdollinenVoittaja = pelitilanne.tarkistaVoitto._1
      println("Arvostellaan tilanne: "+pelitilanne.tarkistaVoitto._2)
      if (mahdollinenVoittaja.isDefined) {
        if (mahdollinenVoittaja.get == itse) 1.0
        else {
          println("Vastustaja on: "+pelitilanne.pelilauta.etsiAuto(pelitilanne.pelaajat(1).auto))
          -1.0
        }
      } //else if (siirrot.size >= 100) -1.0 //RAJA SUORITUKSELLLE
      else {
        0.0
      }
    }
    
    def vaihdaVuoroa() = {
      if (pelitilanne.vuorossa == pelitilanne.pelaajat(0)) pelitilanne.vuorossa = pelitilanne.pelaajat(1)
      else pelitilanne.vuorossa = pelitilanne.pelaajat(0)
      
      pelitilanne.vuorossa.auto.aloitaVuoro()
    }
    
    val siirrotJaArvo = etsiParasSiirtoSarja(kuviteltuTilanne, List[Siirto]())
    println("VALITTIIN "+siirrotJaArvo)
    siirrotJaArvo._1(0).kohdeKoordinaatti
    //itse.auto.sallitutSuunnat(1).muutaSiirroksi(sijainti).kohdeKoordinaatti
    

  }
  
}
package peli

import tietojenTallennus.{TiedostonHallinta, Rata}
import pelikomponentit._
import tietojenTallennus.Profiili

object Peli {
  
  var pelitilanne: Option[Pelitilanne] = None
  
  var rataLista = TiedostonHallinta.haeRadat
  var profiiliLista = TiedostonHallinta.haeProfiilit
  
  def peliKaynnissa = pelitilanne.isDefined
  
  //Luo uuden pelin, tallentaa sen muistiin ja palauttaa
  def uusiPeli(rata: Rata, profiilit: Vector[Option[Profiili]] ): Pelitilanne = {
    
    val pelaajat = profiilit.map(Pelaaja(_)) //Luodaan pelaajat profiilien perusteella.
    val lauta = new Pelilauta(rata.muoto)
    val tilanne = new Pelitilanne(lauta, pelaajat)
    
    tilanne.pelilauta.alustaAutot(pelaajat.map(_.auto))
    
    this.pelitilanne = Some(tilanne)
    
    tilanne
   
  }
  
  def uusiProfiili(profiili: Profiili) {
    val profiiliList: List[Profiili] = profiiliLista.toList 
    profiiliLista = (profiiliList ++ List(profiili)).toVector //Päivitetään oma lista
    tietojenTallennus.TiedostonHallinta.uusiProfiili(profiili) //Päivitetään tiedostoon tallennettu lista
  }
  
}





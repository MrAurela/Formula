package peli

import tietojenTallennus.{TiedostonHallinta, Rata}
import pelikomponentit._
import tietojenTallennus.Profiili

object Peli {
  
  val ai = new Profiili("A.I.") //Nimessä on pisteitä, joten pelaaja ei voi luoda samannimistä profiilia
  
  var pelitilanne: Option[Pelitilanne] = None
  
  var rataLista = TiedostonHallinta.haeRadat
  var profiiliLista = TiedostonHallinta.haeProfiilit
  
  def peliKaynnissa = pelitilanne.isDefined
  
  //Luo uuden pelin, tallentaa sen muistiin ja palauttaa
  def uusiPeli(rata: Rata, profiilit: Vector[Option[Profiili]] ): Pelitilanne = {
    
    val pelaajat = profiilit.map(Pelaaja(_)) //Luodaan pelaajat profiilien perusteella.
    val lauta = new Pelilauta(rata)
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
  
  def uusiRata(rata: Rata) {
    val rataList: List[Rata] = rataLista.toList
    rataLista = (rataLista ++ List(rata)).toVector
    tietojenTallennus.TiedostonHallinta.uusiRata(rata)
  }
  
}





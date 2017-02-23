package peli

import tietojenTallennus.Rata
import pelikomponentit.{Pelilauta, Auto}

object Peli {
  
  var pelitilanne: Option[Pelitilanne] = None
  
  def peliKaynnissa = pelitilanne.isDefined
  
  def uusiPeli(rata: Rata, pelaajat: Vector[Pelaaja]) = {
    val lauta = new Pelilauta(rata.muoto)
    val tilanne = new Pelitilanne(lauta, pelaajat)
    
    tilanne.pelilauta.alustaAutot(pelaajat.map(_.auto))
    
    this.pelitilanne = Some(tilanne)
   
  }
  
  val auto1 = new Auto()
  val auto2 = new Auto()
  
  uusiPeli(new Rata, Vector(new Pelaaja(auto1), new Pelaaja(auto2)))
  
  
}





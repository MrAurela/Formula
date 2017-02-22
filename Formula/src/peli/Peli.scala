package peli

import tietojenTallennus.Rata

import pelikomponentit.Pelilauta

object Peli {
  
  var pelitilanne: Option[Pelitilanne] = None
  
  def peliKaynnissa = pelitilanne.isDefined
  
  def uusiPeli(rata: Rata, pelaajat: Vector[Pelaaja]) = {
    val lauta = new Pelilauta(rata.muoto)
    val tilanne = new Pelitilanne(lauta, pelaajat)
    
    tilanne.pelilauta.alustaAutot()
    
    this.pelitilanne = Some(tilanne)
   
  }
  
  uusiPeli(new Rata, Vector(new Pelaaja(), new Pelaaja()))
  
  
}





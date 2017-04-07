package peli

import tietojenTallennus.{TiedostonHallinta, Rata}
import pelikomponentit._

object Peli {
  
  var pelitilanne: Option[Pelitilanne] = None
  
  val rataLista = TiedostonHallinta.haeRadat
  val profiiliLista = TiedostonHallinta.haeProfiilit
  
  println(rataLista.size)
  rataLista.foreach(println(_))
  println("Radat tulostettu")
  
  println(profiiliLista.size)
  profiiliLista.foreach(println(_))
  println("Profiilit tulostettu!")
  
  def peliKaynnissa = pelitilanne.isDefined
  
  def uusiPeli(rata: Rata, pelaajat: Vector[Pelaaja]) = {
    val lauta = new Pelilauta(rata.muoto)
    val tilanne = new Pelitilanne(lauta, pelaajat)
    
    tilanne.pelilauta.alustaAutot(pelaajat.map(_.auto))
    
    this.pelitilanne = Some(tilanne)
   
  }
  
  
  //KUULUU TESTAUKSEEN
  val auto1 = new Auto()
  val auto2 = new Auto()
  
  
  val muoto: Vector[Vector[Maasto]] = 
    Vector(
      Vector(Reuna, Reuna, Reuna, Reuna, Reuna), 
      Vector(Reuna, Tie, Tie, Tie, Reuna),
      Vector(AloitusRuutu1, AloitusRuutu2, Reuna, Tie, Reuna),
      Vector(Tie, Tie, Tie, Tie, Reuna),
      Vector(Reuna, Reuna, Reuna, Reuna, Reuna)
    )

  uusiPeli(rataLista(0), Vector(new Pelaaja(auto1), new Pelaaja(auto2)))
  
  
}





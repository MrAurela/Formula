package pelikomponentit

class Pelilauta {
  
  val maastot =
    Vector(
      Vector(Reuna, Rata, Rata, Reuna, Reuna), 
      Vector(Reuna, Rata, Rata, Rata, Reuna),
      Vector(Reuna, Rata, Reuna, Rata, Reuna),
      Vector(Reuna, Rata, Rata, Rata, Reuna),
      Vector(Reuna, Reuna, Reuna, Reuna, Reuna)
    )
    
  val ruudut = maastot.map(_.map(new Ruutu(_))) //Vaihdetaan maastot vastaavaan ruutuun
  
  ruudut(1)(0).lisaaAuto(new Auto)
  
}
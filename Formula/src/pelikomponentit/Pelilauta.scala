package pelikomponentit

class Pelilauta(maastot: Vector[Vector[Maasto]]) {
    
  val ruudut = maastot.map(_.map(new Ruutu(_))) //Vaihdetaan maastot vastaavaan ruutuun
  
  ruudut(1)(0).lisaaAuto(new Auto)
  
}
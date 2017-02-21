package peli

import pelikomponentit.Pelilauta

object Peli {
  
  var pelitilanne: Option[Pelitilanne] = Some(new Pelitilanne(new Pelilauta()))
  
}
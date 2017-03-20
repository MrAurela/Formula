package testaus

import org.junit.Test
import org.junit.Assert._
import siirrot._

class pelilauta_lapimentavatKoordinaatit {
  
  @Test def pystySiirto = {
    val siirto = new Siirto(Koordinaatti(0,1), Koordinaatti(0,3))
    assertEquals("Väärät koordinaatit", Vector(Koordinaatti(0,1), Koordinaatti(0,2),Koordinaatti(0,3)),
        siirto.lapimentavatKoordinaatit)
  }
  
  @Test def vaakaSiirto = {
    val siirto = new Siirto(Koordinaatti(1,0), Koordinaatti(3,0))
    assertEquals("Väärät koordinaatit", Vector(Koordinaatti(1,0), Koordinaatti(2,0),Koordinaatti(3,0)),
        siirto.lapimentavatKoordinaatit)
  }
  
  @Test def vinoSiirto = {
    val siirto = new Siirto(Koordinaatti(-1,0), Koordinaatti(2,3))
    assertEquals("Väärät koordinaatit", Vector(Koordinaatti(-1,0), Koordinaatti(0,1),Koordinaatti(1,2), Koordinaatti(2,3)),
        siirto.lapimentavatKoordinaatit)
  }

  @Test def kulmanLapiKulkevaSiirto = {
    val siirto = new Siirto(Koordinaatti(1,0), Koordinaatti(-2,1))
    assertEquals("Väärät koordinaatit", Vector(Koordinaatti(1,0), Koordinaatti(0,0),Koordinaatti(-1,1), Koordinaatti(-2,1)),
        siirto.lapimentavatKoordinaatit)
  }
  
  @Test def osittainVinoSiirto = {
    val siirto = new Siirto(Koordinaatti(-1,0), Koordinaatti(2,2))
    assertEquals("Väärät koordinaatit", Vector(Koordinaatti(-1,0), Koordinaatti(0,0),Koordinaatti(0,1), Koordinaatti(1,1), Koordinaatti(1,2), Koordinaatti(2,2)),
        siirto.lapimentavatKoordinaatit)
  }
 
  @Test def osittainVinoSiirto2 = {
    val siirto = new Siirto(Koordinaatti(-1,0), Koordinaatti(1,1))
    assertEquals("Väärät koordinaatit", Vector(Koordinaatti(-1,0), Koordinaatti(0,0),Koordinaatti(0,1), Koordinaatti(1,1)),
        siirto.lapimentavatKoordinaatit)
  }
  
  @Test def takaperoinenSiirto = {
    val siirto = new Siirto(Koordinaatti(2,-2), Koordinaatti(1,0))
    assertEquals("Väärät koordinaatit", Vector(Koordinaatti(2,-2), Koordinaatti(2,-1), Koordinaatti(1,-1),Koordinaatti(1,0)),
        siirto.lapimentavatKoordinaatit)
  }
  
  @Test def takaperoinenSiirto2 = {
    val siirto = new Siirto(Koordinaatti(3,0), Koordinaatti(1,0))
    assertEquals("Väärät koordinaatit", Vector(Koordinaatti(3,0), Koordinaatti(2,0), Koordinaatti(1,0)),
        siirto.lapimentavatKoordinaatit)
  }

  
  
  
}
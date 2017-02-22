package pelikomponentit

class Ruutu(maasto_ : Maasto) {
  
  val maasto =  maasto_
  var auto: Option[Auto] = None
  
  def onAuto = this.auto.isDefined
  
  def poistaAuto = this.auto = None
  
  def lisaaAuto(auto: Auto) = this.auto = Some(auto)
  
}
package pelikomponentit

class Ruutu(maasto_ : Maasto) {
  
  val maasto =  maasto_
  var auto: Option[Auto] = None
  
  override def toString = maasto.toString + ", " + auto.getOrElse("Ei autoa")
  
  val onMaali = this.maasto.onMaali
  def on(kysyttavaMaasto: Maasto) = this.maasto == kysyttavaMaasto
  
  def voiAjaa = this.maasto != Reuna && this.eiAutoa  //Voiko ruutuun tai sen läpi ajaa
  
  def onAuto = this.auto.isDefined
  def eiAutoa = !this.onAuto
  
  def poistaAuto() = this.auto = None
  
  def lisaaAuto(auto: Auto) = this.auto = Some(auto)
  
}
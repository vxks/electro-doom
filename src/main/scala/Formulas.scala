import Units.*

object Formulas {
  // V = I*R
  def V(I: Current, R: Resistance): Voltage = I * R
  def I(V: Voltage, R: Resistance): Current = V / R
}

import MathUtils.*

import java.math.MathContext

object Units {
  private val precision: MathContext = new MathContext(21)

  opaque type Ohm    = BigDecimal
  opaque type Volt   = BigDecimal
  opaque type Ampere = BigDecimal

  case class Resistance(private[Units] val ohms: Ohm) { self =>
    lazy val inverse: BigDecimal        = BigDecimal(1) / ohms
    def /(bigD: BigDecimal): Resistance = Resistance(ohms / bigD)
    def *(bigD: BigDecimal): Resistance = Resistance(ohms * bigD)
    def +(that: Resistance): Resistance = Resistance(ohms + that.ohms)
    def -(that: Resistance): Resistance = Resistance(ohms - that.ohms)
    override def toString: String       = s"${ohms}Ω"

    override def equals(obj: Any): Boolean = obj match
      case r: Resistance => self.ohms.round(precision) == r.ohms.round(precision)
      case _             => false
  }
  object Resistance {
    given Numeric[Resistance] =
      Numeric[BigDecimal].bimap[Resistance](Resistance(_))(_.ohms)
  }

  case class Voltage(private[Units] val volts: Volt) { self =>
    lazy val inverse: BigDecimal           = BigDecimal(1) / volts
    def /(resistance: Resistance): Current = Current(volts / resistance.ohms)
    def /(current: Current): Resistance    = Resistance(volts / current.amps)
    def /(bigD: BigDecimal): Voltage       = Voltage(volts / bigD)
    override def toString: String          = s"${volts}V"

    override def equals(obj: Any): Boolean = obj match
      case r: Voltage => self.volts.round(precision) == r.volts.round(precision)
      case _          => false
  }
  object Voltage {
    given Numeric[Voltage] =
      Numeric[BigDecimal].bimap[Voltage](Voltage(_))(_.volts)
  }

  case class Current(private[Units] val amps: Ampere) { self =>
    lazy val inverse: BigDecimal           = BigDecimal(1) / amps
    def *(resistance: Resistance): Voltage = Voltage(amps * resistance.ohms)
    def /(bigD: BigDecimal): Current       = Current(amps / bigD)
    override def toString: String          = s"${amps}A"

    override def equals(obj: Any): Boolean = obj match
      case r: Current => self.amps.round(precision) == r.amps.round(precision)
      case _          => false
  }
  object Current {
    given Numeric[Current] =
      Numeric[BigDecimal].bimap[Current](Current(_))(_.amps)
  }

  extension (bigD: BigDecimal)
    def Ohms: Resistance = Resistance(bigD)
    def Volts: Voltage   = Voltage(bigD)
    def Amps: Current    = Current(bigD)

  extension (int: Int)
    def Ohms: Resistance = Resistance(BigDecimal(int))
    def Volts: Voltage   = Voltage(BigDecimal(int))
    def Amps: Current    = Current(BigDecimal(int))

  extension (float: Float)
    def Ohms: Resistance = Resistance(BigDecimal(float))
    def Volts: Voltage   = Voltage(BigDecimal(float))
    def Amps: Current    = Current(BigDecimal(float))

  extension (double: Double)
    def Ohms: Resistance = Resistance(BigDecimal(double))
    def Volts: Voltage   = Voltage(BigDecimal(double))
    def Amps: Current    = Current(BigDecimal(double))
}

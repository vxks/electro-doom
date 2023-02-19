import scala.math

sealed trait Circuit {
  self =>

  import Circuit.*
  import CircuitComponent.*
  import Units.*
  import Resistance.*

  def transform[B](f: CircuitComponent => CircuitComponent): Circuit =
    //TODO: tailrec
    self match
      case Series(cs*)                 => Series(cs.map(_.transform(f)): _*)
      case Parallel(cs*)               => Parallel(cs.map(_.transform(f)): _*)
      case component: CircuitComponent => f(component)

  lazy val resistanceEq: Option[Resistance] =
    self match
      case Series(cs*) =>
        cs.flatMap(_.resistanceEq) match {
          case seq if seq.nonEmpty => Some(seq.sum)
          case _                   => None
        }

      case Parallel(cs*) =>
        val maybeRs = cs.flatMap(_.resistanceEq) match {
          case seq if seq.nonEmpty => Some(seq)
          case _                   => None
        }
        maybeRs.map(rs => (1 / rs.map(_.inverse).sum).Ohms)

      case cc: CircuitComponent =>
        cc match
          case Resistor(_, value)  => Some(value)
          case VoltageSource(_, _) => None

  def collectMeasurements(input: Voltage | Current): Map[Resistor, (Voltage, Current)] = {
    import Formulas.*

    val R_eq = self.resistanceEq
      .getOrElse(throw new IllegalArgumentException("Short circuit, WYD!?!?"))

    val (v_in, i_t) = input match
      case v: Voltage => (v, I(v, R_eq))
      case c: Current => (V(c, R_eq), c)

    //TODO: make tail recursive
    def loop(
      v_t: Voltage,
      i_t: Current,
      cir: Circuit,
      agg: Map[Resistor, (Voltage, Current)]
    ): Map[Resistor, (Voltage, Current)] =
      cir match {
        // I constant
        case Series(cs*) =>
          cs.flatMap { c =>
            val r_c = c.resistanceEq.getOrElse(0.Ohms)
            val i_c = i_t
            val v_c = V(i_c, r_c)
            loop(v_c, i_c, c, Map())
          }.toMap ++ agg

        // V constant
        case Parallel(cs*) =>
          cs.flatMap { c =>
            val r_c = c.resistanceEq.getOrElse(0.Ohms)
            val v_c = v_t
            val i_c = I(v_c, r_c)
            loop(v_c, i_c, c, Map())
          }.toMap ++ agg

        case circuitComponent: CircuitComponent =>
          circuitComponent match
            case r: Resistor      => agg + (r -> (V(i_t, r.value), I(v_t, r.value)))
            case _: VoltageSource => agg
      }

    loop(v_in, i_t, self, Map())
  }

  /**
   * Assumes total voltage drop == V_in
   */
  def collectVoltages(V_in: Voltage): Map[Resistor, Voltage] =
    collectMeasurements(V_in).view.mapValues(_._1).toMap

  def collectCurrents(I_total: Current): Map[Resistor, Current] =
    collectMeasurements(I_total).view.mapValues(_._2).toMap
}

object Circuit:
  case class Series(cs: Circuit*)   extends Circuit
  case class Parallel(cs: Circuit*) extends Circuit

  sealed trait CircuitComponent extends Circuit
  object CircuitComponent {
    case class VoltageSource(id: Long, value: Units.Voltage) extends CircuitComponent
    case class Resistor(id: Long, value: Units.Resistance)   extends CircuitComponent
  }

  val S: Series.type                          = Series
  val P: Parallel.type                        = Parallel
  val VS: CircuitComponent.VoltageSource.type = CircuitComponent.VoltageSource
  val R: CircuitComponent.Resistor.type       = CircuitComponent.Resistor

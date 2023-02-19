import scala.math

sealed trait Circuit {
  self =>

  import Circuit.*
  import Units.*
  import Resistance.*

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

      case Resistor(_, value)      => Some(value)
      case VoltageSource(_, value) => None

  /**
   * Assumes total voltage drop == V_in
   */
  def voltagesC(V_in: Voltage): Map[Resistor, Voltage] = {
    import Formulas.*

    val R_eq = self.resistanceEq
      .getOrElse(throw new IllegalArgumentException("Short circuit, WYD!?!?"))
    val I_t = I(V_in, R_eq)

    //TODO: make tail recursive
    def loop(
      v_t: Voltage,
      i_t: Current,
      cir: Circuit,
      agg: Map[Resistor, Voltage]
    ): Map[Resistor, Voltage] =
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

        case r: Resistor      => agg + (r -> V(i_t, r.value))
        case _: VoltageSource => agg
      }

    loop(V_in, I_t, self, Map())
  }
}

object Circuit:
  case class Series(cs: Circuit*)                          extends Circuit
  case class Parallel(cs: Circuit*)                        extends Circuit
  case class VoltageSource(id: Long, value: Units.Voltage) extends Circuit
  case class Resistor(id: Long, value: Units.Resistance)   extends Circuit

  val S: Series.type         = Series
  val P: Parallel.type       = Parallel
  val VS: VoltageSource.type = VoltageSource
  val R: Resistor.type       = Resistor

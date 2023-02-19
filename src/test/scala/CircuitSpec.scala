import Circuit.*
import Units.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

import scala.util.Random

class CircuitSpec extends AnyFunSpec with Matchers {

  import CircuitSpec.*

  describe("Circuit") {

    describe("voltage") {
      it("must collect series circuit voltage measurements") {
        val expected = Map(
          R(1, 1.0.Ohms) -> 0.125.Volts,
          R(2, 2.0.Ohms) -> 0.25.Volts,
          R(3, 4.0.Ohms) -> 0.5.Volts,
          R(4, 1.0.Ohms) -> 0.125.Volts
        )
        val result = circuitS.voltagesC(1.Volts)
        result mustBe expected
      }

      it("must collect parallel circuit voltage measurements") {
        val rs       = resistors.take(5)
        val expected = rs.map(_ -> 1.0.Volts).toMap
        val result   = P(rs: _*).voltagesC(1.Volts)
        result mustBe expected
      }

      it("must collect mixed circuit voltage measurements") {
        val expected = Map(
          R(1, 1.0.Ohms) -> 0.4.Volts,
          R(2, 1.0.Ohms) -> 0.2.Volts,
          R(3, 1.0.Ohms) -> 0.2.Volts,
          R(4, 1.0.Ohms) -> 0.4.Volts
        )
        val result = circuitMixed.voltagesC(1.Volts)
        result mustBe expected
      }
    }
  }
}

object CircuitSpec:
  private def genResistance(): Resistance = Random.nextFloat().Ohms

  val resistors: LazyList[Resistor] =
    LazyList.iterate(R(1, genResistance())) { r =>
      R(
        id = r.id + 1,
        value = genResistance()
      )
    }

  val circuitS: Circuit = Series(
    R(1, 1.Ohms),
    R(2, 2.Ohms),
    R(3, 4.Ohms),
    R(4, 1.Ohms)
  )
  val circuitP: Circuit = Parallel(
    R(1, 1.Ohms),
    R(2, 2.Ohms),
    R(3, 4.Ohms),
    R(4, 1.Ohms)
  )

  val circuitMixed: Circuit = Series(
    R(1, 1.Ohms),
    Parallel(
      R(2, 1.Ohms),
      R(3, 1.Ohms)
    ),
    R(4, 1.Ohms)
  )

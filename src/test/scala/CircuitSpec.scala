import Circuit.*
import Units.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

import scala.util.Random

class CircuitSpec extends AnyFunSpec with Matchers {

  import CircuitSpec.*

  describe("Circuit") {

    describe("voltages") {
      it("must collect series circuit voltage measurements") {
        val expected = Map(
          R(1, 1.0.Ohms) -> 0.125.Volts,
          R(2, 2.0.Ohms) -> 0.25.Volts,
          R(3, 4.0.Ohms) -> 0.5.Volts,
          R(4, 1.0.Ohms) -> 0.125.Volts
        )
        val result = circuitS.collectVoltages(1.Volts)
        result mustBe expected
      }

      it("must collect parallel circuit voltage measurements") {
        val rs       = resistors.take(5)
        val expected = rs.map(_ -> 1.0.Volts).toMap
        val result   = P(rs: _*).collectVoltages(1.Volts)
        result mustBe expected
      }

      it("must collect mixed circuit voltage measurements") {
        val expected = Map(
          R(1, 1.0.Ohms) -> 0.4.Volts,
          R(2, 1.0.Ohms) -> 0.2.Volts,
          R(3, 1.0.Ohms) -> 0.2.Volts,
          R(4, 1.0.Ohms) -> 0.4.Volts
        )
        val result = circuitMixed1.collectVoltages(1.Volts)
        result mustBe expected
      }
    }

    describe("currents") {
      it("must collect parallel circuit current measurements") {
        val expected = Map(
          R5 -> 0.3.Amps,
          R6 -> 0.2.Amps,
          R7 -> 0.3.Amps,
          R8 -> 0.2.Amps
        )
        val result = circuitP2.collectCurrents(1.Amps)
        result mustBe expected
      }

      it("must collect series circuit current measurements") {
        val rs       = resistors.take(5)
        val expected = rs.map(_ -> 0.1.Amps).toMap
        val result   = S(rs: _*).collectCurrents(0.1.Amps)
        result mustBe expected
      }

      it("must collect mixed circuit current measurements") {
        val expected = Map(
          R1 -> 1.Amps,
          R2 -> 1.Amps,
          R5 -> 0.3.Amps,
          R6 -> 0.2.Amps,
          R7 -> 0.3.Amps,
          R8 -> 0.2.Amps
        )
        val result = circuitMixed2.collectCurrents(1.Amps)
        result mustBe expected
      }
    }
  }
}

object CircuitSpec:
  import CircuitComponent.*
  
  private def genResistance(): Resistance = Random.nextFloat().Ohms

  val resistors: LazyList[Resistor] =
    LazyList.iterate(R(1, genResistance())) { r =>
      R(
        id = r.id + 1,
        value = genResistance()
      )
    }

  val R1 = R(1, 1.Ohms)
  val R2 = R(2, 2.Ohms)
  val R3 = R(3, 4.Ohms)
  val R4 = R(4, 1.Ohms)
  val R5 = R(1, 2.0.Ohms)
  val R6 = R(2, 3.0.Ohms)
  val R7 = R(3, 2.0.Ohms)
  val R8 = R(4, 3.0.Ohms)

  val circuitS: Circuit  = Series(R1, R2, R3, R4)
  val circuitP1: Circuit = Parallel(R1, R2, R3, R4)
  val circuitP2: Circuit = Parallel(R5, R6, R7, R8)

  val circuitMixed1: Circuit = Series(
    R(1, 1.Ohms),
    Parallel(
      R(2, 1.Ohms),
      R(3, 1.Ohms)
    ),
    R(4, 1.Ohms)
  )

  val circuitMixed2: Circuit =
    S(
      R1,
      P(R5, R6, R7, R8),
      R2
    )

package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

import scala.concurrent.duration.FiniteDuration

class SClassic extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with Gradients with BlockS with FieldUtils {
  lazy val grain: Double = node.get("grain")
  override def main(): Any = {
    // An aggregate operation
    val leader = S(grain, nbrRange _)
    // Write access to node state
    node.put("leader", leader)
    // Return value of the program
    leader
  }
}
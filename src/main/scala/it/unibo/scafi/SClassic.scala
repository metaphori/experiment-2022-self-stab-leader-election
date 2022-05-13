package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.incarnation.BlockSFix

import scala.concurrent.duration.FiniteDuration

class SClassic extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with Gradients with BlockS with BlockSFix with FieldUtils {
  lazy val grain: Double = node.get("grain")
  override def main(): Any = {
    // An aggregate operation
    val leader = S(grain, nbrRange _)
    val leaderId = G[ID](leader, if(leader) mid() else -1, identity, nbrRange _)
    // Write access to node state
    node.put("leader", leaderId)
    node.put("leaderEffect", leaderId % 7)
    node.put("isLeader", leaderId == mid())
    // Return value of the program
    leader
  }
}
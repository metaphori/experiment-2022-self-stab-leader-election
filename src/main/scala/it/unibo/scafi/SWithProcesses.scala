package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.incarnation.{BlockSWithProcesses, ProcessFix}

/**
  * Notice: this S program does not work. See comment below.
  */
class SWithProcesses extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with Gradients with FieldUtils with BlockSWithProcesses with BlockG with ProcessFix {
  lazy val grain: Double = node.get("grain")
  override def main(): Any = {
    // An aggregate operation
    val leader = localLeaderElection(id = mid(), symmetryBreaker = mid(), radius = grain)
    // Write access to node state
    node.put("leader", leader)
    node.put("leaderEffect", leader % 7)
    node.put("isLeader", leader == mid())
    // Return value of the program
    leader == mid()
  }
}
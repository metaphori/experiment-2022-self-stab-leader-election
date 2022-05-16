package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.incarnation.{BlockSWithProcesses, ProcessFix}

/**
  * Notice: this S program does not work. See comment below.
  */
class SByGCFeedback extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with Gradients with FieldUtils with BlockG with BlockC {
  lazy val grain: Double = node.get("grain")
  override def main(): Any = {
    // An aggregate operation
    val leader: ID = SbyGCfeedback()
    // Write access to node state
    node.put("leader", leader)
    node.put("leaderEffect", leader % 7)
    node.put("isLeader", leader == mid())
    // Return value of the program
    leader == mid()
  }

  /**
    * The C block, specialized from Viroli et al. (2018), collects and sends to each leader its current estimated diameter D_i
    * by maximizing among the estimated distances \hat{d_i}(t) and diameter estimates at nodes that i constrains
    * @param k A large value accelerates convergence but impairs resilience by delaying recovery from loss of the current leader.
    *          A small value improves resilience but slows convergence or can elect multiple leaders
    * @return
    */
  def SbyGCfeedback(k: Double = 1.0): ID = rep(Msg()) { case Msg(leaderEst, hopCount, propRadius, _) =>
    val m@Msg(leader, hopDistEstimate, _, _) = mabf(leaderEst, propRadius)
    node.put("mabf", m)
    val potential = hopDistEstimate // classicGradient(leader == mid())
    val estimatedDiameter = C[Double, Double](potential, Math.max, hopDistEstimate, 0)
    node.put("c", estimatedDiameter)
    Msg(leader, potential, k * estimatedDiameter, mid())
  }.leaderEstimate

  import Builtins.Bounded
  case class Msg(leaderEstimate: ID = mid(), hopCount: Double = Double.PositiveInfinity, propagationRadius: Double = Double.PositiveInfinity, by: ID = mid())
  object Msg {
    /**
      * The current constraining node is a member of V¯i(t + 1) with
      * (i) the smallest distance estimate \hat{d_j} (t);
      * (ii) if tied, the largest propagation radius Rj (t);
      * (iii) if still tied, the smallest j.
      */
    implicit val ordering = new Ordering[Msg] {
      override def compare(a: Msg, b: Msg): Int = List(
        a.hopCount.compareTo(b.hopCount),
        -(a.propagationRadius.compareTo(b.propagationRadius)),
        a.by.compareTo(b.by),
      ).collectFirst { case x if x!=0 => x }.getOrElse(0)
    }
  }
  import Msg.ordering

  /**
    * MABF decides whether a node i should be a leader, by selecting leader estimates from neighbors j ∈ N (i) that
    * (i) carry distance estimate (hop count) ˆdj that is smaller than the propagation radius ˆdj < Rj , and
    * (ii) their leader σj has a higher priority than i. A node becomes a leader whenever no such neighbor exists.
    * More precisely, define Vi(t + 1) comprising the neighbors of i whose distance estimates are within their propagation radius:
    *   V_i(t + 1) = {j ∈ N (i) | \hat{d_j}(t) < R_j(t)}
    * Then define \overline{V_i}(t + 1) ⊂ Vi(t + 1) comprising members j of V_i(t + 1) that carry the smallest leader estimate σ_j(t) < i.
    * If \overline{V_i}(t + 1) is empty, then i becomes a leader, with a distance estimate of 0, and a propagation radius calculated by the C block
    * If \overline{V_i}(t + 1) is nonempty, i cannot be a leader, and will attach itself to the leader one of the members of V¯ i(t + 1) is attached to.
    * This neighbor also becomes the current constraining node ci(t+ 1) of i, used to update the MABF variables σi(t), \overline{d_i}(t), Ri(t).
    * The current constraining node is a member of \overline{V_i}(t + 1) with
    *     (i) the smallest distance estimate \hat{d_j}(t);
    *     (ii) if tied, the largest propagation radius Rj (t);
    *     (iii) if still tied, the smallest j.
    * Because of the definition of \overline{V_i}, the selected j is the node in V_i that is attached to the highest priority leader.
    * Finally, we set the ci(t+1) to min V˜i(t+1). Each non-leader node then updates its leader, distance, and propagation radius estimates as below with j = c_i(t+1):
    *     σ_i(t+ 1) = σ_j(t), \hat{d_i}(t+1) = \hat{d_j}(t)+ 1, R_i(t+1) = R_j(t).
    * @param leaderEstimate
    * @param propagationRadius
    * @return
    */
  def mabf(leaderEstimate: ID, propagationRadius: Double): Msg =
    rep(Msg(leaderEstimate, 0, propagationRadius)) { case msg =>
      val msgs = includingSelf.reifyField(nbr{ msg })
      node.put("msgs", msgs)
      val filteredMsg = msgs.values.filter { case Msg(l,h,p,_) => h < p && l < mid() }
      node.put("msgs_filtered", filteredMsg)
      val smallestLeaderEst = filteredMsg.minByOption(_.leaderEstimate)
      filteredMsg
        .filter(_.leaderEstimate == smallestLeaderEst.get.leaderEstimate) // filter nbrs attached to higher priority leader
        .minOption // and attach to leader of neighbour with smallest dist estimate etc. (see Ordering[Msg])
        .map(m => m.copy(hopCount = m.hopCount+1, by=mid())) // cf. eq(7)
        .getOrElse(Msg(mid(), 0, propagationRadius))
    }
}
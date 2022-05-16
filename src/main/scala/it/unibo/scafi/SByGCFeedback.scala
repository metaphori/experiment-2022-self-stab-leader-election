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
    *
    * @param k A large value accelerates convergence but impairs resilience by delaying recovery from loss of the current leader.
    *          A small value improves resilience but slows convergence or can elect multiple leaders
    * @return
    */
  def SbyGCfeedback(k: Double = 1.0): ID = rep((Double.PositiveInfinity, mid())) { case (propRadius, _) =>
    val Msg(leader, distanceEstimate, _, _) = mabf(mid(), propRadius)
    val potential = classicGradient(leader == mid())
    val c = C[Double, Double](potential, Math.max, distanceEstimate, 0)
    node.put("c", c)
    (k * c, leader)
  }._2

  import Builtins.Bounded
  case class Msg(leaderEstimate: ID = mid(), hopCount: Double = Double.PositiveInfinity, propagationRadius: Double = Double.PositiveInfinity, by: ID = mid())
  object Msg {
    /**
      * The current constraining node is a member of V¯i(t + 1) with
      * (i) the smallest distance estimate \hat{d_j} (t);
      * (ii) if tied, the largest propagation radius Rj (t);
      * (iii) if still tied, the smallest j.
      */
    implicit val boundedMsg = new Bounded[Msg] {
      override def bottom: Msg = Msg(mid(), 0, 0, mid())
      override def top: Msg = Msg(mid(), Double.PositiveInfinity, 0)
      override def compare(a: Msg, b: Msg): Int = List(
        a.hopCount.compareTo(b.hopCount),
        -(a.propagationRadius.compareTo(b.propagationRadius)),
        a.by.compareTo(b.by),
      ).collectFirst { case x if x!=0 => x }.getOrElse(0)
    }
  }

  import Msg.boundedMsg
  implicit def boundedToOrdering[A: Bounded]: Ordering[A] = (x: A, y: A) => implicitly[Bounded[A]].compare(x, y) // for Scala collection min/minOption

  /**
    * MABF decides whether a node i should be a leader, by selecting leader estimates from neighbors j ∈ N (i) that
    * (i) carry distance estimate (hop count) ˆdj that is smaller than the propagation radius ˆdj < Rj , and
    * (ii) their leader σj has a higher priority than i. A node becomes a leader whenever no such neighbor exists.
    * More precisely, define Vi(t + 1) comprising the neighbors of i whose distance estimates are within their propagation radius:
    *   V_i(t + 1) = {j ∈ N (i) | \hat{d_j} (t) < R_j (t)}
    * @param leaderEstimate
    * @param propagationRadius
    * @return
    */
  def mabf(leaderEstimate: ID, propagationRadius: Double): Msg =
    rep(Msg(leaderEstimate, 0, propagationRadius)) { case msg =>
      val msgs = excludingSelf.reifyField(nbr{ msg })
      node.put("msgs", msgs)
      val filteredMsg = msgs.values.filter{ case Msg(l,h,p,_) => h < p || l >= mid() }
      node.put("msgs_filtered", filteredMsg)
      filteredMsg
        .minOption
        .map(m => m.copy(hopCount = m.hopCount+1))
        .getOrElse(Msg(mid(), 0, propagationRadius))
    }
}
package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.incarnation.BlockSFix

/**
  * Notice: this S program does not work. See comment below.
  */
class SByMirko extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with Gradients with FieldUtils {
  lazy val grain: Double = node.get("grain")
  override def main(): Any = {
    // An aggregate operation
    val leader = S(grain)
    // Write access to node state
    node.put("leader", leader)
    node.put("leaderEffect", leader % 7)
    node.put("isLeader", leader == mid())
    // Return value of the program
    leader == mid()
  }

  import Builtins.Bounded

  case class Msg(distance: Double, id: Int, symBreaker: Int)
  implicit object BoundedMsg extends Bounded[Msg]{
    override def bottom: Msg = Msg(0.0, mid(), mid())
    override def top: Msg = Msg(0.0, mid(), Int.MaxValue)
    override def compare(a: Msg, b: Msg): Int =
      // if (a.symBreaker == b.symBreaker)  a.distance.compareTo(b.distance)  else  a.symBreaker.compareTo(b.symBreaker)
      List(a.symBreaker.compareTo(b.symBreaker), a.distance.compareTo(b.distance), a.id.compareTo(b.id)).collectFirst { case x if x != 0 => x }.getOrElse(0)
  }

  /**
    * This does not work. Two effects can be observed:
    * 1) a node can be recognised by others as leader without considering itself a leader
    * 2) areas are formed basically according to the ID of nodes.. so, if we have a grid network
    * like this.
    *    7  8  9
    *    4  5  6
    *    1  2  3
    *  S would produce something as
    *    4  4  5
    *    1  1  2
    *    1  1  2
    *  i.e. only node 1 would self-recognise as leader and correctly form an area, whereas other
    *  areas would be at the outskirt of area 1, proposing leaders that are inside other areas
    *  and not even leaders there!
    */
  def S(grain: Double): ID =
    rep(Msg(0.0, mid(), mid())){ case Msg(d,i,s) =>
      // node.put("past msg", node.getOrElse("past msg", "") + " " + Msg(d,i,s))
      // node.put("past msg set", node.getOrElse("past msg set", "") + " " + includingSelf.reifyField(Msg(nbr(d),nbr(i),nbr(s))))
      val minMsg = minHoodPlus[Msg]{
        Msg(nbr{d} + nbrRange(), nbr{i}, nbr{s}) match {
          case m@Msg(_, id, _) if (id == mid()) => implicitly[Bounded[Msg]].bottom
          case Msg(dd, _, _) if (dd >= grain) => implicitly[Bounded[Msg]].bottom //top
          case m => m
        }
      }
      val perturbLeader = branch(alchemistTimestamp.toDouble > 500 && alchemistTimestamp.toDouble < 520) {
        rep(alchemistRandomGen.nextInt())(identity)
      } { minMsg.id }
      // node.put("minMsg", minMsg)
      minMsg.copy(id = perturbLeader)
    }.id

}
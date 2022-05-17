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
    val leader = SwithSB(grain, mid())
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
      List(a.symBreaker.compareTo(b.symBreaker), a.distance.compareTo(b.distance), a.id.compareTo(b.id)).collectFirst { case x if x != 0 => x }.getOrElse(0)
  }

  def S(grain: Double): ID =
    rep(Msg(0.0, mid(), mid())){ case Msg(d,i,s) =>
      val minMsg = minHoodPlus[Msg]{
        Msg(nbr{d} + nbrRange(), nbr{i}, nbr{s}) match {
          case Msg(dd, id, _) if id == mid() || dd >= grain => implicitly[Bounded[Msg]].bottom
          case m => m
        }
      }
      val perturbLeader = branch(alchemistTimestamp.toDouble > 500 && alchemistTimestamp.toDouble < 520) {
        rep(alchemistRandomGen.nextInt())(identity)
      } { minMsg.id }
      minMsg.copy(id = perturbLeader)
    }.id

  def SwithSB(grain: Double, symBreaker: Int): ID = {
    val symValue = branch(alchemistTimestamp.toDouble > 500 && alchemistTimestamp.toDouble < 520) {
      rep(alchemistRandomGen.nextInt())(identity)
    } { symBreaker }

    implicit object BoundedMsg extends Bounded[Msg]{
      override def bottom: Msg = Msg(0.0, mid(), symValue)
      override def top: Msg = Msg(0.0, mid(), Int.MaxValue)
      override def compare(a: Msg, b: Msg): Int =
        List(a.symBreaker.compareTo(b.symBreaker), a.distance.compareTo(b.distance), a.id.compareTo(b.id)).collectFirst { case x if x != 0 => x }.getOrElse(0)
    }

    rep(Msg(0.0, mid(), mid())){ case Msg(d,i,s) =>
      minHoodPlus[Msg]{
        Msg(nbr{d} + nbrRange(), nbr{i}, nbr{s}) match {
          case Msg(dd, id, _) if id == mid() || dd >= grain => implicitly[Bounded[Msg]].bottom
          case m => m
        }
      }
    }.id
  }
}
package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.incarnation.BlockSFix

class SByMirko extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with Gradients with FieldUtils {
  lazy val grain: Double = node.get("grain")
  override def main(): Any = {
    // An aggregate operation
    val leader = S(grain)
    // Write access to node state
    node.put("leader", leader == mid())
    // Return value of the program
    leader == mid()
  }

  import Builtins.Bounded

  case class Msg(distance: Double, id: Int, symBreaker: Int)
  implicit object BoundedMsg extends Bounded[Msg]{
    override def bottom: Msg = Msg(0.0, mid(), mid())
    override def top: Msg = Msg(0.0, mid, Int.MaxValue)
    override def compare(a: Msg, b: Msg): Int =
      if (a.symBreaker == b.symBreaker)
        a.distance.compareTo(b.distance)
      else
        a.symBreaker.compareTo(b.symBreaker)
  }

  def S(grain: Double): ID =
    rep(Msg(0.0, mid, mid)){ case Msg(d,i,s) =>
      minHood[Msg]{
        Msg(nbr{d} + nbrRange, nbr{i}, nbr{s}) match {
          case Msg(_, id, _) if (id == mid()) => implicitly[Bounded[Msg]].bottom
          case Msg(dd, _, _) if (dd >= grain) => implicitly[Bounded[Msg]].top
          case m => m
        }
      }
    }.id

}
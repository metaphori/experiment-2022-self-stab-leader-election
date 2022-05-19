package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

/**
  * Notice: this S program does not work. See comment below.
  */
class SMinimisingRep extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with Gradients with FieldUtils {
  lazy val grain: Double = node.get("grain")
  override def main(): Any = {
    // An aggregate operation
    val symBreaker = rep(alchemistRandomGen.nextInt())(identity)
    val leader = SWithMinimisingRep(grain /*mid().doubleValue() / 100*/, mid())
    // Write access to node state
    node.put("leader", leader)
    node.put("leaderEffect", leader % 7)
    node.put("isLeader", leader == mid())
    // Return value of the program
    leader == mid()
  }

  import Builtins.Bounded

  case class Msg(distance: Double, id: Int, symBreaker: Int) {
    def top: Msg = Msg(distance, id, Int.MaxValue)
  }

  def SWithMinimisingRep(grain: Double, symBreaker: Int): ID = {
    implicit object BoundedMsg extends Bounded[Msg]{
      override def bottom: Msg = Msg(0.0, mid(), symBreaker)
      override def top: Msg = Msg(0.0, mid(), Int.MaxValue)
      override def compare(a: Msg, b: Msg): Int =
        List(a.symBreaker.compareTo(b.symBreaker), a.distance.compareTo(b.distance), a.id.compareTo(b.id)).collectFirst { case x if x != 0 => x }.getOrElse(0)
    }

    def fR(curMin: Msg, old: Msg): Msg = curMin
    def fMP(value: Msg): Msg = value match {
      case Msg(dd, id, _) if id == mid() || dd >= grain => implicitly[Bounded[Msg]].top
      case m => m
    }

    rep[Msg](Msg(0.0, mid(), mid())) { case x@Msg(d,i,s) =>
      fR(
        minHoodPlusLoc(implicitly[Bounded[Msg]].bottom)(fMP(Msg(nbr{d} + nbrRange(), nbr{i}, nbr{s}))),
        x
      )
    }.id
  }
}
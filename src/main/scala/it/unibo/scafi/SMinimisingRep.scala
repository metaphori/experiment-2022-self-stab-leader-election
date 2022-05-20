package it.unibo.scafi

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.incarnation.ProcessFix

/**
  * Notice: this S program does not work. See comment below.
  */
class SMinimisingRep extends AggregateProgram with StandardSensors with ScafiAlchemistSupport
  with Gradients with FieldUtils with ProcessFix {
  lazy val grain: Double = node.get("grain")
  override def main(): Any = {
    // An aggregate operation
    val symBreaker = branch(alchemistTimestamp.toDouble.toLong % 1200 < 600) { mid() } {
      rep(alchemistRandomGen.nextInt())(identity)
    }
    val leader = SWithMinimisingRep(grain /*mid().doubleValue() / 100*/, symBreaker)
    // Write access to node state
    node.put("leader", leader)
    node.put("leaderEffect", leader % 7)
    node.put("isLeader", leader == mid())
    // Return value of the program
    leader == mid()
  }

  import Builtins.Bounded

  case class Candidacy(symBreaker: Int, distance: Double, leaderId: Int)

  // cf. https://arxiv.org/pdf/1711.08297.pdf
  def SWithMinimisingRep(grain: Double, symBreaker: Int): ID = {
    implicit object BoundedMsg extends Bounded[Candidacy]{
      override def bottom: Candidacy = Candidacy(implicitly[Bounded[Int]].bottom, implicitly[Bounded[Double]].bottom, implicitly[Bounded[ID]].bottom)
      override def top: Candidacy = Candidacy(implicitly[Bounded[Int]].top, implicitly[Bounded[Double]].top, implicitly[Bounded[ID]].top)
      override def compare(a: Candidacy, b: Candidacy): Int =
        List(a.symBreaker.compareTo(b.symBreaker), a.distance.compareTo(b.distance), a.leaderId.compareTo(b.leaderId)).collectFirst { case x if x != 0 => x }.getOrElse(0)
    }

    def fR(curMin: Candidacy, old: Candidacy): Candidacy = curMin
    def fMP(value: Candidacy): Candidacy = value match {
      case Candidacy(_, dd, id) if id == mid() || dd >= grain => implicitly[Bounded[Candidacy]].top
      case m => m
    }

    val loc = Candidacy(symBreaker, 0.0, mid())
    rep[Candidacy](loc) { case previousCandidacy @ Candidacy(sym, dist, leader) =>
      fR(
        minHoodPlusLoc(loc){
          fMP(Candidacy(nbr{sym}, nbr{dist} + nbrRange(), nbr{leader}))
        },
        previousCandidacy
      )
    }.leaderId
  }

  def SWithMinimisingShare(grain: Double, symBreaker: Int): ID = {
    implicit object BoundedMsg extends Bounded[Candidacy]{
      override def bottom: Candidacy = Candidacy(implicitly[Bounded[Int]].bottom, implicitly[Bounded[Double]].bottom, implicitly[Bounded[ID]].bottom)
      override def top: Candidacy = Candidacy(implicitly[Bounded[Int]].top, implicitly[Bounded[Double]].top, implicitly[Bounded[ID]].top)
      override def compare(a: Candidacy, b: Candidacy): Int =
        List(a.symBreaker.compareTo(b.symBreaker), a.distance.compareTo(b.distance), a.leaderId.compareTo(b.leaderId)).collectFirst { case x if x != 0 => x }.getOrElse(0)
    }

    def fR(curMin: Candidacy, old: Candidacy): Candidacy = curMin
    def fMP(value: Candidacy): Candidacy = value match {
      case Candidacy(_, dd, id) if id == mid() || dd >= grain => implicitly[Bounded[Candidacy]].top
      case m => m
    }

    val loc = Candidacy(symBreaker, 0.0, mid())
    share[Candidacy](loc) { case (previousCandidacy, nbrc) =>
      fR(
        minHoodPlusLoc(loc){
          val nbrCandidacy @ Candidacy(sym, dist, leader) = nbrc()
          fMP(nbrCandidacy.copy(distance = nbrCandidacy.distance + nbrRange()))
        },
        previousCandidacy
      )
    }.leaderId
  }
}
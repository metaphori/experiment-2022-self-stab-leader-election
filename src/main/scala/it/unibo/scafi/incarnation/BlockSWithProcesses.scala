package it.unibo.scafi.incarnation

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

import Ordered._
import Builtins._ // for type classes

trait BlockSWithProcesses {
  self: AggregateProgram with ProcessFix with BlockG with ScafiAlchemistSupport with StandardSensors =>
  import SpawnInterface._ // for statuses
  // Conversions
  implicit def boundedToOrdering[A: Bounded]: Ordering[A] = (x: A, y: A) => implicitly[Bounded[A]].compare(x, y)
  def bounded[A: Bounded]: Bounded[A] = implicitly[Bounded[A]]
  // Data
  /** Represent data produced by a single aggregate process
    * @param symmetryBreaker
    *   the symmetry breaker shared by the leader
    * @param distanceFromLeader
    *   the distance (using a certain metric)
    */
  case class LeaderProcessOutput[S](symmetryBreaker: S, distanceFromLeader: Double)

  /** @param localLeader
    *   the current leader field produce by processes
    * @param localId
    *   local id fields
    * @param breaker
    *   simmetry breaker used during the leader election
    * @param radius
    *   the maxiumum influence area of a leader
    * @param distanceFunction
    *   the strategy used to compute the distance from the leader
    */
  case class LeaderProcessInput[S](
      localLeader: ID,
      localId: ID,
      breaker: S,
      radius: Double,
      distanceFunction: Distance,
      distanceFromLeader: Double
  )

  /** This function produces a field of id resulted from a distributed leader election process.
    * @param id
    *   the id node fields
    * @param symmetryBreaker
    *   simmetry breaker used during the leader election
    * @param radius
    *   the maxiumum influence area of a leader
    * @param distanceFunction
    *   the strategy used to compute the distance from the leader
    * @return
    */
  def localLeaderElection[S: Bounded](
      id: ID = mid(),
      symmetryBreaker: S,
      radius: Double,
      distanceFunction: Distance = Distance(nbrRange, (metric, source) => distanceTo(source, metric))
  ): ID = {
    val default = id -> LeaderProcessOutput[S](symmetryBreaker, 0.0)
    rep(default) { case (leadId, leadOutput) =>
      val candidate = id == leadId
      // compute the leaders using processes, in jointing point multiple leader could exists
      val leaders: Map[ID, LeaderProcessOutput[S]] = sspawn2[ID, LeaderProcessInput[S], LeaderProcessOutput[S]](
        processDefinition,
        mux(candidate)(Set(id))(Set.empty), // a process is spawn only if I am the local candidate
        LeaderProcessInput(leadId, id, symmetryBreaker, radius, distanceFunction, leadOutput.distanceFromLeader)
      )
      // node.put("leaders", leaders)
      val closeEnough = leaders.filter { case (_, LeaderProcessOutput(_, distance)) => distance < radius }
      // node.put("close-enough", closeEnough)
      // choose the leader using the breaking symmetry value
      selectLeader(leaders + default).getOrElse(default)
    }._1
  }

  Predef
  private def processDefinition[S: Bounded]: ID => LeaderProcessInput[S] => POut[LeaderProcessOutput[S]] = id =>
    input => {
      val (status, gradient) = insideBubble(id)(input) // I check this zone is inside the bubble when id is the leader
      optBranch(status == Terminated || status == External) { // if I am external or the process is terminated, return a default field
        POut(LeaderProcessOutput(implicitly[Bounded[S]].bottom, Double.PositiveInfinity), status)
      } {
        // Otherwise, I continue to expand the bubble in this zone
        POut(expandBubble(bounded[S])(gradient)(id)(input), status)
      }
    }

  private def insideBubble[S]: ID => LeaderProcessInput[S] => (Status, Double) =
    processId => { case LeaderProcessInput(localLeader, uid, _, radius, distanceFunction, localDistanceLeader) =>
      val distanceFromLeader = distanceFunction(processId == uid) // distance from the leader
      optBranch(processId == uid && uid != localLeader) {
        // started the process, but I am not the leader anymore, so I suppress that process
        (Terminated, Double.PositiveInfinity)
      } {
        val isEdge = detectedEdge(distanceFromLeader, processId, uid, distanceFunction.metric, localLeader)
        val inBubble = distanceFromLeader <= radius // check if this zone is inside the bubble
        // check if any node near to me have this zone activate
        val status =
          mux(inBubble && !isEdge)(Output)(External)
        // node.put(s"distance-$processId", distanceFromLeader)
        // node.put(s"status-$processId", status)
        (status, distanceFromLeader)
      }
    }

  private def expandBubble[S: Bounded]: Double => ID => LeaderProcessInput[S] => LeaderProcessOutput[S] =
    gradient =>
      processId => { case LeaderProcessInput(_, uid, breaker, _, distanceFunction, _) =>
        val source = processId == uid
        // broadcast the breaker of this leader in the entire zone where
        LeaderProcessOutput(broadcastAlong(source, gradient, breaker, distanceFunction.metric), gradient)
      }

  // select the leader using the symmetric breaker
  private def selectLeader[S: Bounded](
      leaders: Map[ID, LeaderProcessOutput[S]]
  ): Option[(ID, LeaderProcessOutput[S])] = {
    leaders.reduceOption[(ID, LeaderProcessOutput[S])] {
      case (leaderA @ (idA, LeaderProcessOutput(breakerA, _)), leaderB @ (idB, LeaderProcessOutput(breakerB, _))) =>
        if ((breakerA, idA) > (breakerB, idB)) {
          leaderA
        } else {
          leaderB
        }
    }
  }

  def broadcastAlong[D: Bounded](source: Boolean, g: Double, data: D, metric: Metric): D = {
    share(data) { case (l, nbrField) =>
      mux(source)(data)(excludingSelf.minHoodSelector[Double, D](nbr(g) + metric())(nbrField()).getOrElse(l))
    }
  }

  def optBranch[A](cond: Boolean)(th: A)(el: A): A =
    align(vm.index)(_ => align(cond)(mux(_)(th)(el)))

  // TODO simplify this method.
  private def detectedEdge(
      distanceFromLeader: Double,
      processId: ID,
      uid: ID,
      metric: Metric,
      localLeader: ID
  ): Boolean = {
    val leaderHop = share(0) { case (difference, nbrLeaderHop) =>
      mux(processId == uid)(0) { // the leader share the edge from them, 0 in the source zone
        val distanceField = excludingSelf // the neighbourhood field of distances
          .reifyField(nbr(distanceFromLeader + metric()))
        // the node collected before me
        val parents = distanceField.filter(_._2 <= distanceFromLeader).keys.toSet
        // node after me
        val children = distanceField.filter(_._2 > distanceFromLeader).keys.toSet
        // the distances of node collocated before me
        val differenceFieldParent = excludingSelf
          .reifyField(nbrLeaderHop())
          .filter { case (id, _) => parents.contains(id) }
        // the local leader of the nodes before me
        val leaderParentField = includingSelf
          .reifyField(nbr(localLeader))
          .filter { case (id, _) => parents.contains(id) }
        // get the nearest node
        val (parent, parentDifferences) = differenceFieldParent
          .minByOption(_._2)
          .getOrElse((uid, difference))
        // get the field of local leader of the children nodes
        val leaderChildrenField = includingSelf
          .reifyField((nbrLeaderHop(), nbr(localLeader)))
          .filter { case (id, _) => children.contains(id) }
        // node.put(s"parent-$processId", parent)
        // Some parent is direct connected with a leader?
        val directConnectedWithArea = leaderParentField.exists(_._2 == processId)
        // Some children has the same leader of mine and it is direct connected with this area?
        val anyAfterMeDirectConnected =
          leaderChildrenField.values.exists(node => localLeader == node._2 && node._1 == 0)
        val parentLeader = leaderParentField.getOrElse(parent, processId)
        mux(parentLeader != localLeader || parentLeader != processId)(
          // if I have a leader different from whom send me the local leader of the parentLeader has another processId, I put 1
          mux(directConnectedWithArea || anyAfterMeDirectConnected)(1)(parentDifferences + 1)
        )(parentDifferences)
      }
    }
    // node.put(s"edge-$processId", leaderHop)
    leaderHop > 1
  }
}

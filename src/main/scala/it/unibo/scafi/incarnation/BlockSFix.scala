package it.unibo.scafi.incarnation

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

trait BlockSFix {
  self: AggregateProgram with BlockS =>

  /* Fix for S: using classicGradient instead of G */
  override def breakUsingUids(uid: (Double, ID),
                              grain: Double,
                              metric: Metric): Boolean =
  // Initially, each device is a candidate leader, competing for leadership.
    uid == rep(uid) { lead: (Double, ID) =>
      // Distance from current device (uid) to the current leader (lead).
      val dist = origG[Double](uid == lead, 0, (_: Double) + metric(), metric) // classicGradient(uid == lead)

      // Initially, current device is candidate, so the distance ('dist')
      // will be 0; the same will be for other devices.
      // To solve the conflict, devices abdicate in favor of devices with
      // lowest UID, according to 'distanceCompetition'.
      distanceCompetition(dist, lead, uid, grain, metric)
    }

  def origG[V](source: Boolean, field: V, acc: V => V, metric: () => Double): V =
    rep((Double.MaxValue, field)) { case (dist, value) =>
      mux(source) {
        (0.0, field)
      } {
        excludingSelf.minHoodSelector(
          toMinimize = nbr { dist } + metric()
        )(data = (nbr { dist } + metric(), acc(nbr { value }))).getOrElse((Double.PositiveInfinity, field))
      }
    }._2
}

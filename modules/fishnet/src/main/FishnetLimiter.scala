package lila.fishnet

import reactivemongo.api.bson._
import scala.concurrent.duration._

import lila.common.IpAddress
import lila.db.dsl._

final private class FishnetLimiter(
    analysisColl: Coll,
    requesterApi: lila.analyse.RequesterApi
)(implicit ec: scala.concurrent.ExecutionContext) {

  import FishnetLimiter._
  import Analyser._

  def apply(sender: Work.Sender, ignoreConcurrentCheck: Boolean, ownGame: Boolean): Fu[RequestStatus] =
    (fuccess(ignoreConcurrentCheck) >>| concurrentCheck(sender)) flatMap {
      case false => fuccess(RequestStatus.SimulatenousRequest)
      case true => perDayCheck(sender).map { _ match {
        case true => RequestStatus.Ok
        case false => RequestStatus.RateLimited
        }
      }
    } flatMap { requestStatus =>
      ((requestStatus == RequestStatus.Ok) ?? requesterApi.add(sender.userId, ownGame)) inject requestStatus
    }

  private val RequestLimitPerIP = new lila.memo.RateLimit[IpAddress](
    credits = 60,
    duration = 20 hours,
    key = "request_analysis.ip"
  )

  private def concurrentCheck(sender: Work.Sender) =
    sender match {
      case Work.Sender(_, _, mod, system) if mod || system => fuTrue
      case Work.Sender(userId, ip, _, _) =>
        !analysisColl.exists(
          $or(
            $doc("sender.ip"     -> ip),
            $doc("sender.userId" -> userId)
          )
        )
      case _ => fuFalse
    }

private def perDayCheck(sender: Work.Sender) =
    sender match {
      case Work.Sender(_, _, mod, system) if mod || system => fuTrue
      case Work.Sender(userId, ip, _, _) =>
        def perUser =
          requesterApi.countTodayAndThisWeek(userId) map { case (daily, weekly) =>
            weekly < maxPerWeek &&
              daily < (if (weekly < maxPerWeek * 2 / 3) maxPerDay else maxPerDay * 2 / 3)
          }
        ip.fold(perUser) { ipAddress =>
          RequestLimitPerIP(ipAddress, cost = 1)(perUser)(fuFalse)
        }
      case _ => fuFalse
    }
  }

object FishnetLimiter {
  val maxPerDay  = 40
  val maxPerWeek = 200
}

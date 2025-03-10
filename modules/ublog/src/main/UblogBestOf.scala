package lila.ublog

import java.time.{Year, YearMonth, ZoneOffset}

import scala.util.Try

import reactivemongo.api.bson.BSONNull

import lila.db.dsl.{ *, given }
import lila.memo.CacheApi

object UblogBestOf:

  private val ublogOrigin = YearMonth.of(2020, 1) // TODO FIXME check the actual date
  private def currentYearMonth = YearMonth.now(ZoneOffset.UTC)


  def selector(month: YearMonth) =
    val (start, end) = boundsOfMonth(month)
    $doc("topics".$ne(UblogTopic.offTopic), "lived.at".$gt(start).$lt(end))

  private def boundsOfMonth(month: YearMonth): (Instant, Instant) =
    val start = month.atDay(1).atStartOfDay()
    val end   = month.atEndOfMonth().atTime(23, 59, 59)
    (start.toInstant(ZoneOffset.UTC), end.toInstant(ZoneOffset.UTC))


  def readYearMonth(year: Int, month: Int): Option[YearMonth] =
    safeYearMonth(year, month).filter: ym =>
          ym.isAfter(ublogOrigin) || ym == (ublogOrigin) && (ym.isBefore(currentYearMonth) || ym == (currentYearMonth))

  case class WithPosts(yearMonth: YearMonth, posts: List[UblogPost.PreviewPost])

final class UblogBestOfApi(colls: UblogColls, ublogApi: UblogApi, cacheApi: CacheApi)(using Executor):

  import UblogBsonHandlers.{ *, given }

  private val withPostsCache =
    cacheApi[Year, Seq[UblogBestOf.WithPosts]](16, "ublog.bestOf.withPosts"):
      _.refreshAfterWrite(1.hour).buildAsyncFuture: year =>
        (1 to 12).map(x => year.atMonth(x))
        .map: month => 
          ublogApi.aggregateVisiblePosts(UblogBestOf.selector(month), 0, 4).map: preview =>
            UblogBestOf.WithPosts(month, preview)
        .parallel

  //def withPosts: Fu[List[UblogTopic.WithPosts]] = withPostsCache.get {}

private def safeYearMonth(year: Int, month: Int): Option[YearMonth] =
  Try(YearMonth.of(year, month)).toOption
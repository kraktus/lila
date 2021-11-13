package lila.puzzle

import scala.concurrent.ExecutionContext

import lila.db.dsl._
import lila.user.User

// mobile app BC
final class PuzzleBatch(colls: PuzzleColls, anonApi: PuzzleAnon, pathApi: PuzzlePathApi)(implicit
    ec: ExecutionContext
) {

  import BsonHandlers._

  def nextFor(user: Option[User], nb: Int, theme: PuzzleTheme.Key): Fu[Vector[Puzzle]] = (nb > 0) ?? {
    user match {
      case None => anonApi.getBatchFor(nb, theme)
      case Some(user) =>
        {
          val tier =
            if (user.perfs.puzzle.nb > 5000) PuzzleTier.Good
            else PuzzleTier.Top
          pathApi.nextFor(
            user,
            theme,
            tier,
            PuzzleDifficulty.Normal,
            Set.empty
          ) orFail
            s"No puzzle path for ${user.id} $tier" flatMap { pathId =>
              colls.path {
                _.aggregateList(nb) { framework =>
                  import framework._
                  Match($id(pathId)) -> List(
                    Project($doc("puzzleId" -> "$ids", "_id" -> false)),
                    Unwind("puzzleId"),
                    Sample(nb),
                    PipelineOperator(
                      $lookup.simple(
                        from = colls.puzzle,
                        local = "puzzleId",
                        foreign = "_id",
                        as = "puzzle"
                      )
                    ),
                    PipelineOperator(
                      $doc("$replaceWith" -> $doc("$arrayElemAt" -> $arr("$puzzle", 0)))
                    )
                  )
                }.map {
                  _.view.flatMap(PuzzleBSONReader.readOpt).toVector
                }
              }
            }
        }.mon(_.puzzle.selector.user.batch(nb = nb))
    }
  }
}

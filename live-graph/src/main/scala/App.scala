package se.randomserver

import cats.syntax.all.{*, given}
import cats.effect.syntax.all.{*, given}

import scala.concurrent.ExecutionContext.global
import cats.effect.{ExitCode, IO, IOApp, Ref}
import se.randomserver.geometry.Graph
import se.randomserver.linear.V2
import linear.syntax.{*, given}
import geometry.syntax.{*, given}

import cats.data.OptionT
import fs2.Pipe

import java.util.UUID

object App extends IOApp:
  type ID = UUID
  def path(graphRef: Ref[IO, Graph[V2, Double]], lastRef: Ref[IO, Map[ID, Point[V2, Double]]]) : Pipe[IO, (ID, Point[V2, Double]), Unit] = stream =>
    stream.evalTap {
      case (id, value) => for
        last <- lastRef.get.map(_.get(id))

      yield ()
    }.map(_ => ())


  override def run(args: List[String]): IO[ExitCode] = for
    graph <- Ref.of[IO, Graph[V2, Double]](Graph[V2, Double]())
  yield ExitCode.Success

end App

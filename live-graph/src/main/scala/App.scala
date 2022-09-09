package se.randomserver

import cats.syntax.all.{*, given}
import cats.effect.syntax.all.{*, given}

import scala.concurrent.ExecutionContext.global
import cats.effect.{ExitCode, IO, IOApp, Ref}
import se.randomserver.geometry.{Graph, GraphState, LineSegment}
import se.randomserver.linear.{Additive, LinearIntegral, Metric, V2}
import linear.syntax.{*, given}
import geometry.syntax.{*, given}

import cats.{Apply, Foldable, Functor}
import cats.data.{OptionT, State}
import cats.effect.IO.{IOCont, Uncancelable}
import fs2.{Pipe, Stream}

import java.util.UUID

object App extends IOApp:
  type ID = UUID

  override def run(args: List[String]): IO[ExitCode] = for
    graphRef <- Ref.of[IO, Graph[V2, Double]](Graph[V2, Double]())
    path <- graphRef.modifyState {
      for {
        n1 <- Graph.addPoint(Point(V2(0.0, 0.0)))
        n2 <- Graph.addPoint(Point(V2(1.0, 0.0)))
        n22 <- Graph.addPoint(Point(V2(1.0, 1.0)))
        n23 <- Graph.addPoint(Point(V2(2.0, 2.0)))
        n3 <- Graph.addPoint(Point(V2(2.0, 0.0)))
        e1 <- Graph.addEdge(n1, n2)
        e11 <- Graph.addEdge(n2, n22)
        e12 <- Graph.addEdge(n22, n23)
        e2 <- Graph.addEdge(n2, n3)
        state <- Graph.get[V2, Double]
        path = state.pathWhile(e1)(e => state.edges(e).to == n23).map { edgIds =>
          edgIds.map { a =>
            val Graph.Edge(from, to) = state.edges(a)
            LineSegment(state.vertices(from), state.vertices(to))
          }
        }
      } yield path
    }
    _ <- IO {
      println(path)
    }

  yield ExitCode.Success

end App

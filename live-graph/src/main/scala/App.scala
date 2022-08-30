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
import cats.effect.IO.{IOCont, Uncancelable}
import fs2.{Pipe, Stream}

import java.util.UUID

object App extends IOApp:
  type ID = UUID
  def lastPipe(lastRef: Ref[IO, Map[ID, Graph.NodeId]]) : Pipe[IO, (ID, Point[V2, Double]), (ID, Option[Graph.NodeId], Point[V2, Double])] = stream =>
    stream.evalMap {
      case (id, value) => for
        last <- lastRef.get.map(_.get(id))
      yield (id, last, value)
    }

  def insertPipe(grapRef: Ref[IO, Graph[V2, Double]]): Pipe[IO, (ID, Option[Graph.NodeId], Point[V2, Double]), (ID, Graph.NodeId)] = stream =>
    stream.evalMapFilter {
      case (id, lastIdO, nextPoint) => for // Vehicle has been some where before
        graph <- grapRef.get
        dLastPoint    = lastIdO.map(lastId => lastId -> qdA(graph.vertices(lastId), nextPoint))
        dClosestEdge  = graph.closestEdge(nextPoint)
        dClosestPoint = graph.closestVertex(nextPoint)

        newPointId <- (dLastPoint, dClosestEdge, dClosestPoint) match
          case (Some((lastId, dl)), Some((edgeId, de)), Some((pointId, dp))) => // we come from a point and there are both points and edges present (common case)
            if de < dp && de < dl && de < 1.0 then // closest to a edge within 1 m
              grapRef.modify { graph => graph.insert(nextPoint, edgeId).get }
            else if dl < de && dl < 1.0 then // closer to last point than any other point within 1 m
              val updated = lerp(0.4, graph.vertices(lastId), nextPoint)
              grapRef.modify { graph => graph.update(lastId, updated) }
            else if dp < dl && dp < 1.0 then // closer to some other point within 1 m
              val updated = lerp(0.4, graph.vertices(pointId), nextPoint)
              grapRef.modify { graph => graph.update(pointId, updated) }
            else // more than 1 m from any edge or point create a new edge to lastPoint
              grapRef.modify { graph =>
                val (ng, np) = graph.addPoint(nextPoint)
                val (nng, _) = ng.addEdge(lastId, np)
                nng -> np
              }
          case (Some((lastId, dl)), None, Some((pointId, dp))) => // We come from apoint and there are no edges in graph
            if dl < dp && dl < 1.0 then // within 1 m from last point
              val updated = lerp(0.4, graph.vertices(lastId), nextPoint)
              grapRef.modify { graph => graph.update(lastId, updated) }
            else if dp < 1.0 then
              val updated = lerp(0.4, graph.vertices(pointId), nextPoint)
              grapRef.modify { graph => graph.update(pointId, updated) }
            else
              grapRef.modify { graph =>
                val (ng, np) = graph.addPoint(nextPoint)
                val (nng, _) = ng.addEdge(lastId, np)
                nng -> np
              }
          case (None, Some((edgeId, de)), Some((pointId, dp))) =>
            if de < dp && de < 1.0 then
              grapRef.modify { graph => graph.insert(nextPoint, edgeId).get }
            else if dp < 1.0 then
              val updated = lerp(0.4, graph.vertices(pointId), nextPoint)
              grapRef.modify { graph => graph.update(pointId, updated) }
            else // Weird case just add a point
              grapRef.modify { graph => graph.addPoint(nextPoint) }
          case (None, None, None) =>
            grapRef.modify { graph => graph.addPoint(nextPoint) }
      yield Some(id -> newPointId)
    }

  def updateLast(lastRef: Ref[IO, Map[ID, Graph.NodeId]]): Pipe[IO, (ID, Graph.NodeId), Unit] = stream => {
    stream.evalMap {
      case (id, id1) => lastRef.update(last => last.updated(id, id1))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = for
    last  <- Ref.of[IO, Map[ID, Graph.NodeId]](Map.empty)
    graph <- Ref.of[IO, Graph[V2, Double]](Graph[V2, Double]())
    id = UUID.randomUUID()
    stream = Stream(V2(0.0, 0.0), V2(0.6, 0.1), V2(1.5, 0.0), V2(2.0, 2.0))
      .map(id -> Point(_))
      .through(lastPipe(last))
      .through(insertPipe(graph))
      .through(updateLast(last))
    _ <- stream.compile.drain
    graph <- graph.get
    _ = println(graph.segments)
  yield ExitCode.Success

end App

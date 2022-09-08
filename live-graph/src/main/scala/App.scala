package se.randomserver

import cats.syntax.all.{*, given}
import cats.effect.syntax.all.{*, given}

import scala.concurrent.ExecutionContext.global
import cats.effect.{ExitCode, IO, IOApp, Ref}
import se.randomserver.geometry.{Graph, GraphState}
import se.randomserver.linear.V2
import linear.syntax.{*, given}
import geometry.syntax.{*, given}

import cats.data.{OptionT, State}
import cats.effect.IO.{IOCont, Uncancelable}
import fs2.{Pipe, Stream}

import java.util.UUID

object App extends IOApp:
  type ID = UUID
  //Idef lastPipe(lastRef: Ref[IO, Map[ID, Graph.NodeId]]) : Pipe[IO, (ID, Point[V2, Double]), (ID, Option[Graph.NodeId], Point[V2, Double])] = stream =>
  //I  stream.evalMap {
  //I    case (id, value) => for
  //I      last <- lastRef.get.map(_.get(id))
  //I    yield (id, last, value)
  //I  }

  //Idef insertPipe(grapRef: Ref[IO, Graph[V2, Double]]): Pipe[IO, (ID, Option[Graph.NodeId], Point[V2, Double]), (ID, Graph.NodeId)] = stream =>
  //I  stream.evalMapFilter {
  //I    case (id, lastIdO, nextPoint) => for // Vehicle has been some where before
  //I      graph <- grapRef.get
  //I      dLastPoint    = lastIdO.map(lastId => lastId -> qdA(graph.vertices(lastId), nextPoint))
  //I      dClosestEdge  = graph.closestEdge(nextPoint)
  //I      dClosestPoint = graph.closestVertex(nextPoint)

  //I      newPointId <- (dLastPoint, dClosestEdge, dClosestPoint) match
  //I        case (Some((lastId, dl)), Some((edgeId, de)), Some((pointId, dp))) => // we come from a point and there are both points and edges present (common case)
  //I          if de < dp && de < dl && de < 1.0 then // closest to a edge within 1 m
  //I            grapRef.modify { graph => graph.insert(nextPoint, edgeId).get }
  //I          else if dl < de && dl < 1.0 then // closer to last point than any other point within 1 m
  //I            val updated = lerp(0.4, graph.vertices(lastId), nextPoint)
  //I            grapRef.modify { graph => graph.update(lastId, updated) }
  //I          else if dp < dl && dp < 1.0 then // closer to some other point within 1 m
  //I            val updated = lerp(0.4, graph.vertices(pointId), nextPoint)
  //I            grapRef.modify { graph => graph.update(pointId, updated) }
  //I          else // more than 1 m from any edge or point create a new edge to lastPoint
  //I            grapRef.modify { graph =>
  //I              val (ng, np) = graph.addPoint(nextPoint)
  //I              val (nng, _) = ng.addEdge(lastId, np)
  //I              nng -> np
  //I            }
  //I        case (Some((lastId, dl)), None, Some((pointId, dp))) => // We come from apoint and there are no edges in graph
  //I          if dl < dp && dl < 1.0 then // within 1 m from last point
  //I            val updated = lerp(0.4, graph.vertices(lastId), nextPoint)
  //I            grapRef.modify { graph => graph.update(lastId, updated) }
  //I          else if dp < 1.0 then
  //I            val updated = lerp(0.4, graph.vertices(pointId), nextPoint)
  //I            grapRef.modify { graph => graph.update(pointId, updated) }
  //I          else
  //I            grapRef.modify { graph =>
  //I              val (ng, np) = graph.addPoint(nextPoint)
  //I              val (nng, _) = ng.addEdge(lastId, np)
  //I              nng -> np
  //I            }
  //I        case (None, Some((edgeId, de)), Some((pointId, dp))) =>
  //I          if de < dp && de < 1.0 then
  //I            grapRef.modify { graph => graph.insert(nextPoint, edgeId).get }
  //I          else if dp < 1.0 then
  //I            val updated = lerp(0.4, graph.vertices(pointId), nextPoint)
  //I            grapRef.modify { graph => graph.update(pointId, updated) }
  //I          else // Weird case just add a point
  //I            grapRef.modify { graph => graph.addPoint(nextPoint) }
  //I        case (None, None, None) =>
  //I          grapRef.modify { graph => graph.addPoint(nextPoint) }
  //I    yield Some(id -> newPointId)
  //I  }

  //Idef updateLast(lastRef: Ref[IO, Map[ID, Graph.NodeId]]): Pipe[IO, (ID, Graph.NodeId), Unit] = stream => {
  //I  stream.evalMap {
  //I    case (id, id1) => lastRef.update(last => last.updated(id, id1))
  //I  }
  //I}

  override def run(args: List[String]): IO[ExitCode] = for
    //last  <- Ref.of[IO, Map[ID, Graph.NodeId]](Map.empty)
    //graph <- Ref.of[IO, Graph[V2, Double]](Graph[V2, Double]())
    //id = UUID.randomUUID()
    //stream = Stream(V2(0.0, 0.0), V2(0.6, 0.1), V2(1.5, 0.0), V2(2.0, 2.0))
    //  .map(id -> Point(_))
    //  .through(lastPipe(last))
    //  .through(insertPipe(graph))
    //  .through(updateLast(last))
    //_ <- stream.compile.drain
    //graph <- graph.get
    //_ = println(graph.segments)
    graphRef <- Ref.of[IO, Graph[V2, Long]](Graph[V2, Long])


    v <- graphRef.modifyState(Graph.vertices)

  yield ExitCode.Success

end App

package graph

import cats._
import vector.crossZ
import linear.{*, given}
import cats.syntax.{given, *}

import java.util.UUID

opaque type NodeId = UUID

object NodeId:
  def apply(): NodeId = UUID.randomUUID()

opaque type EdgeId = UUID

object EdgeId:
  def apply(): EdgeId = UUID.randomUUID()

case class Edge(from: NodeId, to: NodeId)


def distanceLineSegmentQ[P[_]: Metric: Additive: Apply](p1: P[Double], p2: P[Double], p: P[Double]): Double =
  val l2 = Metric[P].qd(p1, p2)
  if (l2 == 0.0) Metric[P].qd(p, p1)
  else
    val t = Math.max(0.0, Math.min(1.0, Metric[P].dot(p - p1, p2 - p1) / l2))
    val proj = p1 + (p2 - p1) * t
    Metric[P].qd(p, proj)

case class Graph[P[_]: Metric: Additive: Apply](vertices: Map[NodeId,P[Double]], edges: Map[EdgeId, Edge]):
  def closestEdge(p: P[Double]): Option[(EdgeId, Double)] = edges.map {
    case (id, Edge(from, to)) => id -> distanceLineSegmentQ(vertices(from), vertices(to), p)
  }.minByOption(_._2)

  def closestVertex(p: P[Double]): Option[(NodeId, Double)] = vertices.map {
    case (vertexId, vertex) => vertexId -> Metric[P].qd(p, vertex)
  }.minByOption(_._2)

  def insert(p: P[Double], edgeId: EdgeId): Option[Graph[P]] = edges.get(edgeId).map {
    case Edge(from, to) =>
      val id = NodeId()
      val e1 = Edge(from, id)
      val e2 = Edge(id, to)
      copy(
        vertices.updated(id, p),
        edges.removed(edgeId)
          .updated(EdgeId(), e1)
          .updated(EdgeId(), e2)
      )
  }
package se.randomserver
package geometry

import linear.{Additive, Floating, Metric}

import cats.{Apply, Foldable}
import linear.Affine.Point
import geometry.LineSegment
import linear.syntax.{*, given}
import geometry.syntax.{*, given}

object Graph:
  import java.util.UUID

  opaque type NodeId = UUID

  object NodeId:
    def apply(): NodeId = UUID.randomUUID()

  opaque type EdgeId = UUID

  object EdgeId:
    def apply(): EdgeId = UUID.randomUUID()

  case class Edge(from: NodeId, to: NodeId)
end Graph

import Graph.*

case class Graph[P[_]: Foldable: Apply: Metric: Additive, A: Numeric: Floating](vertices: Map[NodeId,Point[P, A]] = Map.empty[NodeId, Point[P, A]], edges: Map[EdgeId, Edge] = Map.empty[EdgeId, Edge]):
  def closestEdge(p: Point[P, A]): Option[(EdgeId, A)] = edges.map {
    case (id, Edge(from, to)) => id -> qdSegment(p, LineSegment(vertices(from), vertices(to)))
  }.minByOption(_._2)

  def closestVertex(p: Point[P, A]): Option[(NodeId, A)] = vertices.map {
    case (vertexId, vertex) => vertexId -> qdA(p, vertex)
  }.minByOption(_._2)

  def addPoint(p: Point[P, A]): (NodeId, Graph[P, A]) =
    val nodeId = NodeId()
    nodeId -> copy(
      vertices.updated(nodeId, p)
    )

  def insert(p: Point[P, A], edgeId: EdgeId): Option[Graph[P, A]] = edges.get(edgeId).map {
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


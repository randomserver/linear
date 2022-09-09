package se.randomserver
package geometry

import linear.{Additive, LinearIntegral, Metric}

import cats.{Applicative, Apply, Eval, FlatMap, Foldable}
import linear.Affine.Point
import geometry.LineSegment
import linear.syntax.{*, given}
import geometry.syntax.{*, given}

import cats.data.{IndexedStateT, State, StateT}
import cats.syntax.applicative.{*, given}

import Graph.*

type GraphState[P[_], A, R] = State[Graph[P,A], R]


case class Graph[P[_]: Foldable: Apply: Metric: Additive, A: LinearIntegral](vertices: Map[NodeId,Point[P, A]] = Map.empty[NodeId, Point[P, A]], edges: Map[EdgeId, Edge] = Map.empty[EdgeId, Edge]):
  def closestEdge(p: Point[P, A]): Option[(EdgeId, A)] = edges.map {
    case (id, Edge(from, to)) => id -> qdSegment(p, LineSegment(vertices(from), vertices(to)))
  }.minByOption(_._2)

  def closestVertex(p: Point[P, A]): Option[(NodeId, A)] = vertices.map {
    case (vertexId, vertex) => vertexId -> qdA(p, vertex)
  }.minByOption(_._2)

  def addPoint(p: Point[P, A]): (Graph[P, A], NodeId) =
    val nodeId = NodeId()
    copy(
      vertices.updated(nodeId, p)
    ) -> nodeId

  def addEdge(from: NodeId, to: NodeId): (Graph[P, A], EdgeId) =
    val edgeId = EdgeId()
    copy(
      edges = edges.updated(edgeId, Edge(from, to))
    ) -> edgeId

  def insert(p: Point[P, A], edgeId: EdgeId): Option[(Graph[P, A], NodeId)] = edges.get(edgeId).map {
    case Edge(from, to) =>
      val id = NodeId()
      val e1 = Edge(from, id)
      val e2 = Edge(id, to)
      copy(
        vertices.updated(id, p),
        edges.removed(edgeId)
          .updated(EdgeId(), e1)
          .updated(EdgeId(), e2)
      ) -> id
  }
  def update(nodeId: NodeId,point: Point[P, A]): (Graph[P, A], NodeId) = copy(
    vertices = vertices.updated(nodeId, point)
  ) -> nodeId

  def next(lastId: NodeId): Set[EdgeId] = edges.collect {
    case (id, edge) if edge.from == lastId => id
  }.toSet

  def continuation(edgeId: EdgeId): Set[EdgeId] = edges.get(edgeId) match
    case None => Set.empty
    case Some(fromEdge) => edges.collect {
      case (id, edge) if fromEdge.to == edge.from => id
    }.toSet

  def segments: Set[LineSegment[P, A]] = edges.map {
    case _ -> Edge(from, to) => LineSegment(vertices(from), vertices(to))
  }.toSet
end Graph

object Graph:
  import java.util.UUID

  opaque type NodeId = UUID

  object NodeId:
    def apply(): NodeId = UUID.randomUUID()

  opaque type EdgeId = UUID

  object EdgeId:
    def apply(): EdgeId = UUID.randomUUID()

  case class Edge(from: NodeId, to: NodeId)


  def apply[P[_]: Foldable: Apply: Metric: Additive, A: LinearIntegral]: Graph[P, A] = Graph()

  def run[P[_], A, R](initial: Graph[P, A])(f: GraphState[P, A, R]): (Graph[P, A], R) = f.run(initial).value

  def get[P[_], A]: GraphState[P, A, Graph[P, A]] = State.get[Graph[P, A]]

  def pure[P[_], A, R](a: R): GraphState[P, A, R] = State.pure[Graph[P, A], R](a)

  def addPoint[P[_], A](p: Point[P, A]): GraphState[P, A, NodeId] = State[Graph[P, A], NodeId](graph => graph.addPoint(p))
  def addEdge[P[_], A](n1: NodeId, n2: NodeId): GraphState[P, A, EdgeId] = State[Graph[P, A], EdgeId](graph => graph.addEdge(n1, n2))

  def insert[P[_], A](p: Point[P ,A], e: EdgeId): GraphState[P, A, Option[NodeId]] =
    State[Graph[P, A], Option[NodeId]](graph => graph.insert(p, e).map(i => i._1 -> Some(i._2)).getOrElse(graph -> None))

  def update[P[_], A](n: NodeId, p: Point[P, A]): GraphState[P, A, NodeId] = State[Graph[P, A], NodeId](graph => graph.update(n, p))

  def closestEdge[P[_], A](p: Point[P, A]): GraphState[P, A, Option[(EdgeId, A)]] =
    State[Graph[P, A], Option[(EdgeId, A)]](graph => (graph -> graph.closestEdge(p)))

  def closestVertex[P[_], A](p: Point[P, A]): GraphState[P, A, Option[(NodeId, A)]] = State[Graph[P, A], Option[(NodeId, A)]](graph => (graph -> graph.closestVertex(p)))

  def edges[P[_], A]: GraphState[P, A, Map[EdgeId, Edge]]  = State.get[Graph[P, A]].map(s => s.edges)

  def vertices[P[_], A]: GraphState[P, A, Map[NodeId, Point[P, A]]] = State.get[Graph[P, A]].map(s => s.vertices)

  def edge[P[_], A](edgeId: EdgeId): GraphState[P, A, Option[Edge]] = State.get[Graph[P, A]].map(_.edges.get(edgeId))

  def vertex[P[_], A](nodeId: NodeId): GraphState[P, A, Option[Point[P, A]]] = State.get[Graph[P, A]].map(_.vertices.get(nodeId))
end Graph
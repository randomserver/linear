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

type GraphStateT[F[_], P[_], A, R] = StateT[F, Graph[P, A], R]
type GraphState[P[_], A, R] = GraphStateT[Eval, P, A, R]


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
  def runF[F[_]: Applicative: FlatMap, P[_], A, R](initial: Graph[P, A])(f: GraphStateT[F, P, A, R]): F[(Graph[P, A], R)] = f.run(initial)

  def liftF[F[_]: Applicative, P[_], A, R](fr: F[R]): IndexedStateT[F, Graph[P, A], Graph[P, A], R] = StateT.liftF[F, Graph[P, A], R](fr)

  def addPoint[F[_]: Applicative, P[_], A](p: Point[P, A]): GraphStateT[F, P, A, NodeId] = StateT(graph => graph.addPoint(p).pure[F])
  def addEdge[F[_]: Applicative, P[_], A](n1: NodeId, n2: NodeId): GraphStateT[F, P, A, EdgeId] = StateT(graph => graph.addEdge(n1, n2).pure[F])

  def insert[F[_]: Applicative, P[_], A](p: Point[P ,A], e: EdgeId): GraphStateT[F, P, A, Option[NodeId]] =
    StateT(graph => graph.insert(p, e).map(i => i._1 -> Some(i._2)).getOrElse(graph -> None).pure[F])

  def update[F[_]: Applicative, P[_], A](n: NodeId, p: Point[P, A]): GraphStateT[F, P, A, NodeId] = StateT(graph => graph.update(n, p).pure[F])

  def closestEdge[F[_]: Applicative, P[_], A](p: Point[P, A]): GraphStateT[F, P, A, Option[(EdgeId, A)]] = StateT(graph => (graph -> graph.closestEdge(p)).pure[F])

  def closestVertex[F[_]: Applicative,P[_], A](p: Point[P, A]): GraphStateT[F, P, A, Option[(NodeId, A)]] = StateT(graph => (graph -> graph.closestVertex(p)).pure[F])

  def edges[F[_]: Applicative, P[_], A]: GraphStateT[F, P, A, Map[EdgeId, Edge]]  = StateT.get.map(s => s.edges)

  def vertices[F[_]: Applicative, P[_], A]: GraphStateT[F, P, A, Map[NodeId, Point[P, A]]] = StateT.get.map(s => s.vertices)





end Graph
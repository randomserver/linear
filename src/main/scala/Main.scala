package se.randomserver

import linear.Affine.Point
import linear.syntax.{*, given}
import linear.Vector.{*, given}
import linear.{Additive, Affine, Finite, Floating, Metric, V2, V3}
import geometry.{IsIntersectableWith, Line, LineSegment}
import geometry.syntax.{*, given}

import cats.syntax.show.{*, given}
import cats.{Applicative, Show}
import cats.Functor

import scala.language.implicitConversions

object Main extends App {
  //val l1 = Line(Point(V2(0.0,0.0)), V2(1.0, 1.0))
  //val l2 = Line(Point(V2(0.0, 1.0)), V2(2.0, 0.0))
  //val p = Point(V2(0.000000, 10.011))

  val l2 = LineSegment(Point(V2(2.0, 2.0)), Point(V2(0.0, 0.0)))
  val l1 = LineSegment(Point(V2(0.7, 0.7)), Point(V2(0.5, 0.5)))

  val intersection = intersect(l1, l2)

  println(intersection)
}

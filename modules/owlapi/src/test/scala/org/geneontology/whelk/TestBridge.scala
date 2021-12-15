package org.geneontology.whelk

import org.geneontology.whelk.Bridge.ListExtensions
import org.phenoscape.scowl._
import utest._

object TestBridge extends TestSuite {

  val tests = Tests {
    "Intersections of size 1 should be unwrapped" - {
      val term = Class("urn:uuid:1")
      val singleton = ObjectIntersectionOf(term)
      val convertedIntersection = Bridge.convertExpression(singleton)
      val convertedTerm = Bridge.convertExpression(term)
      assert(convertedIntersection == convertedTerm)
    }

    "Sequence method on lists" - {
      val a = List(Some(1), None, Some(2), Some(3))
      assert(a.sequence == None)
      val b = List(Some(1), Some(2), Some(3))
      assert(b.sequence == Some(List(1, 2, 3)))
      val c = List[Option[Int]](None)
      assert(c.sequence == None)
    }

  }

}

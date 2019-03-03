package org.geneontology.whelk

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

  }

}

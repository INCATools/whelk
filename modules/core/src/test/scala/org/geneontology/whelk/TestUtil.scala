package org.geneontology.whelk

import utest._
import Util.MapExtensions

object TestUtil extends TestSuite {

  val tests = Tests {
    "Maps of sets should combine" - {
      val a = Map(
        1 -> Set(1, 2, 3),
        2 -> Set.empty[Int],
        3 -> Set(1)
      )
      val b = Map(
        4 -> Set(4, 5, 6),
        1 -> Set(3, 5, 7),
        2 -> Set(7)
      )
      val ab = Map(
        1 -> Set(1, 2, 3, 5, 7),
        2 -> Set(7),
        3 -> Set(1),
        4 -> Set(4, 5, 6),
      )
      assert((a |+| b) == ab)
    }
  }

}

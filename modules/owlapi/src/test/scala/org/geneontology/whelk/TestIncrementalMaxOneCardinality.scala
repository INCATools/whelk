package org.geneontology.whelk

import scala.util.chaining._
import utest._

object TestIncrementalMaxOneCardinality extends TestSuite {

  val tests: Tests = Tests {
    "Max 1 cardinality inferences should work in any order" - {
      val A = AtomicConcept("http://example.org/A")
      val B = AtomicConcept("http://example.org/B")
      val C = AtomicConcept("http://example.org/C")
      val D = AtomicConcept("http://example.org/D")
      val r = Role("http://example.org/r")
      val Max1rC = MaxCardinalityRestriction(r, C, 1)
      val x = Nominal(Individual("http://example.org/x"))
      val y = Nominal(Individual("http://example.org/y"))
      val z = Nominal(Individual("http://example.org/z"))
      val ASubMax1rC = ConceptInclusion(A, Max1rC)
      val BSubA = ConceptInclusion(B, A)
      val xTypeB = ConceptInclusion(x, B)
      val DSubC = ConceptInclusion(D, C)
      val yTypeD = ConceptInclusion(y, D)
      val zTypeD = ConceptInclusion(z, D)
      val xry = ConceptInclusion(x, ExistentialRestriction(r, y))
      val xrz = ConceptInclusion(x, ExistentialRestriction(r, z))
      Reasoner.assert(Set(ASubMax1rC))
        .pipe(Reasoner.assert(Set(BSubA), _))
        .pipe(Reasoner.assert(Set(xTypeB), _))
        .pipe(Reasoner.assert(Set(DSubC), _))
        .pipe(Reasoner.assert(Set(xry), _))
        .pipe(Reasoner.assert(Set(xrz), _))
        .pipe(Reasoner.assert(Set(yTypeD), _))
        .pipe(Reasoner.assert(Set(zTypeD), _))
        .pipe { whelk =>
          assert(whelk.closureSubsBySubclass(y)(z))
          assert(whelk.closureSubsBySubclass(z)(y))
        }
      Reasoner.assert(Set(ASubMax1rC))
        .pipe(Reasoner.assert(Set(yTypeD), _))
        .pipe(Reasoner.assert(Set(zTypeD), _))
        .pipe(Reasoner.assert(Set(BSubA), _))
        .pipe(Reasoner.assert(Set(xTypeB), _))
        .pipe(Reasoner.assert(Set(xry), _))
        .pipe(Reasoner.assert(Set(xrz), _))
        .pipe(Reasoner.assert(Set(DSubC), _))
        .pipe { whelk =>
          assert(whelk.closureSubsBySubclass(y)(z))
          assert(whelk.closureSubsBySubclass(z)(y))
        }
      Reasoner.assert(Set(DSubC))
        .pipe(Reasoner.assert(Set(yTypeD), _))
        .pipe(Reasoner.assert(Set(zTypeD), _))
        .pipe(Reasoner.assert(Set(BSubA), _))
        .pipe(Reasoner.assert(Set(xTypeB), _))
        .pipe(Reasoner.assert(Set(xry), _))
        .pipe(Reasoner.assert(Set(xrz), _))
        .pipe(Reasoner.assert(Set(ASubMax1rC), _))
        .pipe { whelk =>
          assert(whelk.closureSubsBySubclass(y)(z))
          assert(whelk.closureSubsBySubclass(z)(y))
        }
      Reasoner.assert(Set(ASubMax1rC))
        .pipe(Reasoner.assert(Set(yTypeD), _))
        .pipe(Reasoner.assert(Set(zTypeD), _))
        .pipe(Reasoner.assert(Set(BSubA), _))
        .pipe(Reasoner.assert(Set(xTypeB), _))
        .pipe(Reasoner.assert(Set(DSubC), _))
        .pipe(Reasoner.assert(Set(xry), _))
        .pipe(Reasoner.assert(Set(xrz), _))
        .pipe { whelk =>
          assert(whelk.closureSubsBySubclass(y)(z))
          assert(whelk.closureSubsBySubclass(z)(y))
        }
    }
  }

}

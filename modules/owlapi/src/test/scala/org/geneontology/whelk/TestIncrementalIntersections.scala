package org.geneontology.whelk

import org.semanticweb.owlapi.apibinding.OWLManager
import utest._

object TestIncrementalIntersections extends TestSuite {

  val tests: Tests = Tests {
    "New intersection definitions should be properly classified" - {
      val manager = OWLManager.createOWLOntologyManager()
      val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream("incremental-intersections.ofn"))
      val axioms = Bridge.ontologyToAxioms(ontology)
      val whelk = Reasoner.assert(axioms)
      val A = AtomicConcept("http://example.org/A")
      val E = AtomicConcept("http://example.org/E")
      val B = AtomicConcept("http://example.org/B")
      val EC = AtomicConcept("http://example.org/EC")
      val FandC = AtomicConcept("http://example.org/FandC")
      val HI = AtomicConcept("http://example.org/HI")
      val EandB = Conjunction(E, B)
      val EB = AtomicConcept("http://example.org/EB")
      val newAxioms = Set(ConceptInclusion(EB, EandB), ConceptInclusion(EandB, EB))
      val newWhelk = Reasoner.assert(newAxioms, whelk)
      val subclasses = newWhelk.closureSubsBySuperclass(EB)
      assert(subclasses(EC))
      assert(subclasses(FandC))
      assert(subclasses(HI))
      val superclasses = newWhelk.closureSubsBySubclass(EB)
      assert(superclasses(A))
      assert(superclasses(B))
      assert(superclasses(E))
    }
  }


}

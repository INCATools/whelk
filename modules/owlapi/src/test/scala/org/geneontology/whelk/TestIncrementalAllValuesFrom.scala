package org.geneontology.whelk

import org.semanticweb.owlapi.apibinding.OWLManager
import utest._

object TestIncrementalAllValuesFrom extends TestSuite {

  val tests: Tests = Tests {
    "New allValuesFrom subsumptions should classify individuals" - {
      val manager = OWLManager.createOWLOntologyManager()
      val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream("incremental-allvaluesfrom.ofn"))
      val axioms = Bridge.ontologyToAxioms(ontology)
      val whelk = Reasoner.assert(axioms)
      val B = AtomicConcept("http://example.org/B")
      val C = AtomicConcept("http://example.org/C")
      val D = AtomicConcept("http://example.org/D")
      val x = Nominal(Individual("http://example.org/x"))
      val y = Nominal(Individual("http://example.org/y"))
      val newAxioms = Set(ConceptInclusion(C, D))
      val newWhelk = Reasoner.assert(newAxioms, whelk)
      val subclasses = newWhelk.closureSubsBySuperclass(B)
      assert(subclasses(y))
    }
  }

}

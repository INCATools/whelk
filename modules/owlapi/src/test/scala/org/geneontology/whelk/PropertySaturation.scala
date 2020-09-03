package org.geneontology.whelk

import org.semanticweb.owlapi.apibinding.OWLManager
import utest._

object PropertySaturation extends TestSuite {

  val tests = Tests {

    "Property saturation should handle reciprocal subproperties" - {
      val manager = OWLManager.createOWLOntologyManager()
      val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream("property-test.ofn"))
      val axioms = Bridge.ontologyToAxioms(ontology)
      val done = Reasoner.assert(axioms)
    }

  }

}

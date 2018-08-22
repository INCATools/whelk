package org.geneontology.whelk

import utest._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.geneontology.whelk.owlapi.WhelkOWLReasonerFactory
import org.phenoscape.scowl._
import scala.collection.JavaConverters._

object TestOWLReasoner extends TestSuite {

  val tests = Tests {

    "OWLReasoner inferences" - {
      val manager = OWLManager.createOWLOntologyManager()
      val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream("586fc17a00001662-merged.ofn"))
      val reasoner = new WhelkOWLReasonerFactory().createReasoner(ontology)
      val Process = Class("http://purl.obolibrary.org/obo/BFO_0000015")
      assert(reasoner.getTypes(NamedIndividual("http://model.geneontology.org/586fc17a00001662/586fc17a00001739"), false).getFlattened.asScala(Process))
      assert(reasoner.getInstances(Process, false).getFlattened.size == 6)
      assert(reasoner.isConsistent())
    }

    "OWLReasoner inferences B" - {
      val manager = OWLManager.createOWLOntologyManager()
      val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream("test-owl-reasoner-ops.ofn"))
      val reasoner = new WhelkOWLReasonerFactory().createReasoner(ontology)
      assert(reasoner.getTypes(NamedIndividual("http://example.org/test/ab"), true).getFlattened.asScala.toSet ==
        Set(Class("http://example.org/test/AandB")))
      assert(reasoner.getTypes(NamedIndividual("http://example.org/test/ab"), false).getFlattened.asScala.toSet ==
        Set(Class("http://example.org/test/AandB"), Class("http://example.org/test/A"), Class("http://example.org/test/B"), Class("http://example.org/test/C"), OWLThing))
      assert(reasoner.getInstances(Class("http://example.org/test/C"), false).getFlattened.asScala(NamedIndividual("http://example.org/test/ab")))
      assert(!reasoner.getInstances(Class("http://example.org/test/C"), true).getFlattened.asScala(NamedIndividual("http://example.org/test/ab")))
      assert(reasoner.getEquivalentClasses(Class("http://example.org/test/A") and Class("http://example.org/test/B")).getEntities.asScala ==
        Set(Class("http://example.org/test/AandB")))
      assert(reasoner.getSuperClasses(Class("http://example.org/test/A") and Class("http://example.org/test/B"), true).getFlattened.asScala.toSet ==
        Set(Class("http://example.org/test/A"), Class("http://example.org/test/B")))
      assert(reasoner.getSuperClasses(Class("http://example.org/test/A") and Class("http://example.org/test/B"), false).getFlattened.asScala.toSet ==
        Set(Class("http://example.org/test/A"), Class("http://example.org/test/B"), Class("http://example.org/test/C"), OWLThing))
      assert(reasoner.getSubClasses(Class("http://example.org/test/A") and Class("http://example.org/test/B"), true).getFlattened.asScala.toSet ==
        Set(Class("http://example.org/test/D")))
      assert(reasoner.getSubClasses(Class("http://example.org/test/A") and Class("http://example.org/test/B"), false).getFlattened.asScala.toSet ==
        Set(Class("http://example.org/test/D"), Class("http://example.org/test/E"), OWLNothing))
      assert(reasoner.getSubClasses(Class("http://example.org/test/B"), true).getFlattened.asScala.toSet ==
        Set(Class("http://example.org/test/AandB")))
      assert(reasoner.getSubClasses(Class("http://example.org/test/B"), false).getFlattened.asScala.toSet ==
        Set(Class("http://example.org/test/AandB"), Class("http://example.org/test/D"), Class("http://example.org/test/E"), OWLNothing))
      assert(reasoner.getObjectPropertyValues(NamedIndividual("http://example.org/test/ab"), ObjectProperty("http://example.org/test/r")).getFlattened.asScala ==
        Set(NamedIndividual("http://example.org/test/c")))
      assert(reasoner.getObjectPropertyValues(NamedIndividual("http://example.org/test/ab"), ObjectProperty("http://example.org/test/s")).getFlattened.asScala ==
        Set(NamedIndividual("http://example.org/test/c")))
      assert(reasoner.getObjectPropertyValues(NamedIndividual("http://example.org/test/c"), ObjectInverseOf(ObjectProperty("http://example.org/test/s"))).getFlattened.asScala ==
        Set(NamedIndividual("http://example.org/test/ab")))
      assert(reasoner.getObjectPropertyValues(NamedIndividual("http://example.org/test/c"), ObjectProperty("http://example.org/test/q")).getFlattened.asScala ==
        Set(NamedIndividual("http://example.org/test/ab")))
    }

    "OWLReasoner test unknown entities" - {
      val manager = OWLManager.createOWLOntologyManager()
      val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream("test-owl-reasoner-ops.ofn"))
      val reasoner = new WhelkOWLReasonerFactory().createReasoner(ontology)
      reasoner.getSubClasses(Class("http://example.org/blah"), false)
      reasoner.getSuperClasses(Class("http://example.org/blah"), false)
      reasoner.getEquivalentClasses(Class("http://example.org/blah"))
      reasoner.getTypes(NamedIndividual("http://example.org/blah"), false)
      reasoner.getInstances(Class("http://example.org/blah"), false)
      reasoner.getInstances(Class("http://example.org/blah"), true)
    }

  }

}
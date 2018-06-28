package org.geneontology.whelk

import utest._
import org.semanticweb.owlapi.model.OWLOntologyManager
import org.semanticweb.owlapi.apibinding.OWLManager
import org.geneontology.whelk.BuiltIn._
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.phenoscape.scowl._
import scala.collection.JavaConverters._

object TestInferences extends TestSuite {

  val tests = Tests {

    "Inferences should match ELK" - {
      "uberon-tiny.ofn" - compareWhelkAndELK()
      "skeletons.ofn" - compareWhelkAndELK()
      "part-of-arm.ofn" - compareWhelkAndELK()
      "disjunction.ofn" - compareWhelkAndELK()
    }
  }

  def compareWhelkAndELK()(implicit path: utest.framework.TestPath): Unit = {
    val fileName = path.value.last
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(fileName))
    val axioms = Bridge.ontologyToAxioms(ontology)
    val done = Reasoner.assert(axioms)
    val namedSubs = done.subs.filter {
      case ConceptInclusion(sub: AtomicConcept, sup: AtomicConcept) if (sub != sup && sub != Bottom && sup != Top) => true
      case _ => false
    }
    val elk = new ElkReasonerFactory().createReasoner(ontology)
    val terms = axioms.collect { case ax: ConceptInclusion => ax }.flatMap(_.signature).collect { case e: AtomicConcept => e }
    val elkConceptInclusions = terms.filterNot(_ == Top).flatMap(t => elk.getSubClasses(Class(t.id), false).getFlattened.asScala.map(sub => ConceptInclusion(AtomicConcept(sub.getIRI.toString), t))).filterNot(_.subclass == Bottom)
    elk.dispose()
    assert(namedSubs == elkConceptInclusions)

  }

}
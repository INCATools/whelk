package org.geneontology.whelk

import org.geneontology.whelk.BuiltIn._
import org.phenoscape.scowl._
import org.semanticweb.HermiT.ReasonerFactory
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util.{InferredAxiomGenerator, InferredClassAssertionAxiomGenerator, InferredOntologyGenerator, InferredPropertyAssertionGenerator}
import utest._

import scala.collection.JavaConverters._

object TestInferences extends TestSuite {

  val tests = Tests {

    "Inferences should match ELK" - {
      "uberon-tiny.ofn" - compareWhelkAndELK()
      "uberon-tiny-equiv-prop.ofn" - compareWhelkAndELK()
      "skeletons.ofn" - compareWhelkAndELK()
      "part-of-arm.ofn" - compareWhelkAndELK()
      "disjunction.ofn" - compareWhelkAndELK()
      "586fc17a00001662-merged.ofn" - compareWhelkAndHermiTAboxes()
      "insulin_secretion.ofn" - compareWhelkAndHermiTAboxes()
      "swrl-test.ofn" - compareWhelkAndHermiTAboxes()
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
      case _                                                                                                       => false
    }
    val elk = new ElkReasonerFactory().createReasoner(ontology)
    val terms = axioms.collect { case ax: ConceptInclusion => ax }.flatMap(_.signature).collect { case e: AtomicConcept => e }
    val elkConceptInclusions = terms.filterNot(_ == Top).flatMap(t => elk.getSubClasses(Class(t.id), false).getFlattened.asScala.map(sub => ConceptInclusion(AtomicConcept(sub.getIRI.toString), t))).filterNot(_.subclass == Bottom)
    elk.dispose()
    assert(namedSubs == elkConceptInclusions)
  }

  def compareWhelkAndHermiTAboxes()(implicit path: utest.framework.TestPath): Unit = {
    val fileName = path.value.last
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(fileName))
    val axioms = Bridge.ontologyToAxioms(ontology)
    val done = Reasoner.assert(axioms)
    val hermit = new ReasonerFactory().createReasoner(ontology)
    val generator = new InferredOntologyGenerator(hermit, List[InferredAxiomGenerator[_]](
      new InferredPropertyAssertionGenerator(),
      new InferredClassAssertionAxiomGenerator()).asJava)
    generator.fillOntology(manager.getOWLDataFactory, ontology)
    val hermitClassAssertions = (for {
      ClassAssertion(_, Class(cls), NamedIndividual(ind)) <- ontology.getAxioms(Imports.INCLUDED).asScala
    } yield ConceptAssertion(AtomicConcept(cls.toString), Individual(ind.toString))).toSet
      .filterNot(_.concept == Top)
    val hermitRoleAssertions = (for {
      ObjectPropertyAssertion(_, ObjectProperty(prop), NamedIndividual(subject), NamedIndividual(target)) <- ontology.getAxioms(Imports.INCLUDED).asScala
    } yield RoleAssertion(Role(prop.toString), Individual(subject.toString), Individual(target.toString))).toSet
    hermit.dispose()
    val whelkClassAssertions = done.classAssertions.filterNot(_.concept == Top)
    val whelkRoleAssertions = done.roleAssertions
    assert(whelkClassAssertions == hermitClassAssertions)
    assert(whelkRoleAssertions == hermitRoleAssertions)
  }

}
package org.geneontology.whelk

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.Individual
import org.phenoscape.scowl._
import org.semanticweb.HermiT.ReasonerFactory
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util._
import utest._

import scala.collection.JavaConverters._

object TestInferences extends TestSuite {

  val tests = Tests {
    "Inferences should match ELK or HermiT" - {
      "uberon-tiny.ofn" - compareWhelkAndELK()
      "uberon-tiny-equiv-prop.ofn" - compareWhelkAndELK()
      "skeletons.ofn" - compareWhelkAndELK()
      "part-of-arm.ofn" - compareWhelkAndELK()
      "part-of-arm-reflexivity.ofn" - compareWhelkAndHermiT(false)
      "disjunction.ofn" - compareWhelkAndELK()
      "586fc17a00001662-merged.ofn" - compareWhelkAndHermiT(false)
      "insulin_secretion.ofn" - compareWhelkAndHermiT(false)
      "swrl-test.ofn" - compareWhelkAndHermiT(false)
      "self-restrictions.ofn" - compareWhelkAndHermiT(false)
      "unsatisfiable.ofn" - compareWhelkAndELK()
      "taxon-unions.ofn" - compareWhelkAndHermiT(false)
      "unions.ofn" - compareWhelkAndHermiT(false)
    }
  }

  def compareWhelkAndELK()(implicit path: utest.framework.TestPath): Unit = {
    val fileName = path.value.last
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(fileName))
    val axioms = Bridge.ontologyToAxioms(ontology)
    val done = Reasoner.assert(axioms)
    val namedSubs = done.subs.filter {
      case ConceptInclusion(sub: AtomicConcept, sup: AtomicConcept) if (sub != sup && sub != Bottom && sup != Top && sup != Bottom) => true
      case _                                                                                                                        => false
    }
    val whelkEquivs = done.computeTaxonomy.filterNot(_._1 == Top).map { case (term, (equivalents, _)) => equivalents + term }.toSet
    val whelkUnsatisfiable = done.subs.filter(_.superclass == Bottom).map(_.subclass).filterNot(_ == Bottom).collect { case e: AtomicConcept => e }
    val elk = new ElkReasonerFactory().createReasoner(ontology)
    val terms = axioms.collect { case ax: ConceptInclusion => ax }.flatMap(_.signature).collect { case e: AtomicConcept => e }
    val elkConceptInclusions = terms.filterNot(_ == Top).flatMap(t => elk.getSubClasses(Class(t.id), false).getFlattened.asScala.map(sub => ConceptInclusion(AtomicConcept(sub.getIRI.toString), t))).filterNot(_.subclass == Bottom)
    val elkEquivs = (terms + Bottom).map(t => elk.getEquivalentClasses(Class(t.id)).getEntities.asScala.toSet[OWLClass].map(t => AtomicConcept(t.getIRI.toString)))
    val elkUnsatisfiable = elk.getUnsatisfiableClasses.getEntitiesMinusBottom.asScala.toSet[OWLClass].map(c => AtomicConcept(c.getIRI.toString))
    elk.dispose()
    assert(namedSubs == elkConceptInclusions)
    assert(whelkEquivs == elkEquivs)
    assert(whelkUnsatisfiable == elkUnsatisfiable)
  }

  def compareWhelkAndHermiT(aboxOnly: Boolean)(implicit path: utest.framework.TestPath): Unit = {
    val fileName = path.value.last
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(fileName))
    val axioms = Bridge.ontologyToAxioms(ontology)
    val terms = axioms.collect { case ax: ConceptInclusion => ax }.flatMap(_.signature).collect { case e: AtomicConcept => e }
    val done = Reasoner.assert(axioms)
    val hermit = new ReasonerFactory().createReasoner(ontology)
    val generator = new InferredOntologyGenerator(hermit, List[InferredAxiomGenerator[_]](
      new InferredSubClassAxiomGenerator(),
      new InferredEquivalentClassAxiomGenerator(),
      new InferredPropertyAssertionGenerator(),
      new InferredClassAssertionAxiomGenerator()).asJava)
    generator.fillOntology(manager.getOWLDataFactory, ontology)
    val hermitConceptInclusions = terms.filterNot(_ == Top).flatMap(t => hermit.getSubClasses(Class(t.id), false).getFlattened.asScala.map(sub => ConceptInclusion(AtomicConcept(sub.getIRI.toString), t))).filterNot(_.subclass == Bottom)
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
    val whelkSubClassAxioms = done.subs.filter {
      case ConceptInclusion(sub: AtomicConcept, sup: AtomicConcept) if (sub != sup && sub != Bottom && sup != Top) => true
      case _                                                                                                       => false
    }
    if (!aboxOnly) assert(whelkSubClassAxioms == hermitConceptInclusions)
    assert(whelkClassAssertions == hermitClassAssertions)
    assert(whelkRoleAssertions == hermitRoleAssertions)
  }

}
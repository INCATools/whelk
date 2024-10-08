package org.geneontology.whelk

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.{Individual => WIndividual}
import org.phenoscape.scowl._
import org.semanticweb.HermiT.ReasonerFactory
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util._
import utest._

import scala.jdk.CollectionConverters._

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
      "long-chains.ofn" - compareWhelkAndELK()
      "part-of-arm-ranges.ofn" - compareWhelkAndHermiT(false)
      "part-of-arm-ranges2.ofn" - compareWhelkAndHermiT(false)
      "owlrl/eq-ref.ofn" - compareWhelkAndHermiT(true)
      "owlrl/eq-sym.ofn" - compareWhelkAndHermiT(true)
      "owlrl/eq-trans.ofn" - compareWhelkAndHermiT(true)
      "owlrl/eq-rep.ofn" - compareWhelkAndHermiT(true)
      "owlrl/eq-diff.ofn" - compareWhelkAndHermiT(true, true)
      "owlrl/prp-dom.ofn" - compareWhelkAndHermiT(true)
      "owlrl/prp-rng.ofn" - compareWhelkAndHermiT(true)
      "owlrl/prp-fp.ofn" - compareWhelkAndHermiT(true)
      "owlrl/prp-ifp.ofn" - compareWhelkAndHermiT(true)
      "owlrl/prp-irp.ofn" - compareWhelkAndHermiT(true, true)
      "owlrl/prp-symp.ofn" - compareWhelkAndHermiT(true)
      "owlrl/prp-asyp.ofn" - compareWhelkAndHermiT(true, true)
      "owlrl/prp-trp.ofn" - compareWhelkAndHermiT(true)
      "owlrl/prp-spo.ofn" - compareWhelkAndHermiT(true)
      "owlrl/prp-eqp.ofn" - compareWhelkAndHermiT(true)
      "owlrl/prp-pdw-adp.ofn" - compareWhelkAndHermiT(true, true)
      "owlrl/prp-inv.ofn" - compareWhelkAndHermiT(true)
      "owlrl/prp-npa.ofn" - compareWhelkAndHermiT(true, true)
      "owlrl/cls-int.ofn" - compareWhelkAndHermiT(true)
      "owlrl/cls-uni.ofn" - compareWhelkAndHermiT(true)
      "owlrl/cls-com.ofn" - compareWhelkAndHermiT(true, true)
      "owlrl/cls-svf.ofn" - compareWhelkAndHermiT(true)
      "owlrl/cls-avf.ofn" - compareWhelkAndHermiT(true)
      "owlrl/cls-hv.ofn" - compareWhelkAndHermiT(true)
      "owlrl/cls-maxqc0.ofn" - compareWhelkAndHermiT(true, true)
      "owlrl/cls-maxqc1.ofn" - compareWhelkAndHermiT(true)
      "owlrl/cls-oo.ofn" - compareWhelkAndHermiT(true)
      "owlrl/cax-sco.ofn" - compareWhelkAndHermiT(true)
      "owlrl/cax-dw-adc.ofn" - compareWhelkAndHermiT(true, true)
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

  def compareWhelkAndHermiT(aboxOnly: Boolean, inconsistent: Boolean = false)(implicit path: utest.framework.TestPath): Unit = {
    val fileName = path.value.last
    val manager = OWLManager.createOWLOntologyManager()
    val ontology = manager.loadOntologyFromOntologyDocument(this.getClass.getResourceAsStream(fileName))
    val axioms = Bridge.ontologyToAxioms(ontology)
    val terms = axioms.collect { case ax: ConceptInclusion => ax }.flatMap(_.signature).collect { case e: AtomicConcept => e }
    val whelk = Reasoner.assert(axioms)
    val hermit = new ReasonerFactory().createReasoner(ontology)
    if (inconsistent) {
      assert(!hermit.isConsistent)
      assert(!whelk.closureSubsBySuperclass(Bottom).forall {
        case n: Nominal => false
        case _          => true
      })
    } else {
      val generator = new InferredOntologyGenerator(hermit, List[InferredAxiomGenerator[_]](
        new InferredSubClassAxiomGenerator(),
        new InferredEquivalentClassAxiomGenerator(),
        new InferredPropertyAssertionGenerator(),
        new InferredClassAssertionAxiomGenerator()).asJava)
      generator.fillOntology(manager.getOWLDataFactory, ontology)
      val hermitConceptInclusions = terms.filterNot(_ == Top).flatMap(t => hermit.getSubClasses(Class(t.id), false).getFlattened.asScala.map(sub => ConceptInclusion(AtomicConcept(sub.getIRI.toString), t))).filterNot(_.subclass == Bottom)
      val hermitClassAssertions = (for {
        ClassAssertion(_, Class(cls), NamedIndividual(ind)) <- ontology.getAxioms(Imports.INCLUDED).asScala
      } yield ConceptAssertion(AtomicConcept(cls.toString), WIndividual(ind.toString))).toSet
        .filterNot(_.concept == Top)
      val hermitRoleAssertions = (for {
        ObjectPropertyAssertion(_, ObjectProperty(prop), NamedIndividual(subject), NamedIndividual(target)) <- ontology.getAxioms(Imports.INCLUDED).asScala
      } yield RoleAssertion(Role(prop.toString), WIndividual(subject.toString), WIndividual(target.toString))).toSet
      hermit.dispose()
      val whelkClassAssertions = whelk.classAssertions.filterNot(_.concept == Top)
      val whelkRoleAssertions = whelk.roleAssertions
      val whelkSubClassAxioms = whelk.subs.filter {
        case ConceptInclusion(sub: AtomicConcept, sup: AtomicConcept) if (sub != sup && sub != Bottom && sup != Top) => true
        case _                                                                                                       => false
      }
      if (!aboxOnly) assert(whelkSubClassAxioms == hermitConceptInclusions)
      assert(whelkClassAssertions == hermitClassAssertions)
      assert(whelkRoleAssertions == hermitRoleAssertions)
    }
  }

}
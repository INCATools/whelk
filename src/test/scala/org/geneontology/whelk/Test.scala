package org.geneontology.whelk

import scala.collection.immutable.Queue
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import java.io.File
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.phenoscape.scowl._
import scala.collection.JavaConverters._
import BuiltIn.Bottom

object Test extends App {

  val A = AtomicConcept("A")
  val B = AtomicConcept("B")
  val C = AtomicConcept("C")
  val D = AtomicConcept("D")
  val E = AtomicConcept("E")
  val F = AtomicConcept("F")
  val G = AtomicConcept("G")
  val H = AtomicConcept("H")
  val I = AtomicConcept("I")
  val R = Role("R")
  val S = Role("S")

  val axioms = Set[Axiom](
    ConceptInclusion(A, B),
    ConceptInclusion(B, C),
    ConceptInclusion(D, E),
    ConceptInclusion(B, ExistentialRestriction(R, D)),
    ConceptInclusion(ExistentialRestriction(R, E), F),
    ConceptInclusion(G, Conjunction(C, F)),
    ConceptInclusion(Conjunction(C, F), G),
    ConceptInclusion(D, ExistentialRestriction(S, H)),
    ConceptInclusion(H, ExistentialRestriction(S, I)),
    ConceptInclusion(ExistentialRestriction(S, I), H),
    RoleInclusion(R, S))

  val reasoner1 = Reasoner.index(axioms, Reasoner.empty).copy(
    roleComps = Map((S, S) -> Set(S)))

  //val reasoner = Reasoner.prepare(Bridge.ontologyToAxioms(OWLManager.createOWLOntologyManager().loadOntology(IRI.create("http://purl.obolibrary.org/obo/pato.owl"))))
  val ontology = OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("uberon-trimmed.ofn")))
  val uberonAxioms = Bridge.ontologyToAxioms(ontology)
  //val goAxioms = Bridge.ontologyToAxioms(OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("../../Source/obo-asserted/go.owl"))))
  //val reasoner = Reasoner.prepare(Bridge.ontologyToAxioms(OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("skeletons.ofn")))))
  println("Start")
  val start = System.currentTimeMillis
  val done = ReteReasoner.assert(uberonAxioms)
  val stop = System.currentTimeMillis
  println(s"Reasoned in: ${stop - start} ms")

  //done.subs.foreach(println)
  println("================")
  //done.subs.collect { case (ci @ ConceptInclusion(AtomicConcept(_), AtomicConcept(_))) => ci }.foreach(println)
  //println(done.concIncs.size)
  //println(done.subs.size - reasoner.concIncs.size)
  println(done.subs.size)
  //done.subs.foreach(println)
  val named = done.subs.filter {
    case ConceptInclusion(sub: AtomicConcept, sup: AtomicConcept) if (sub != sup && sub != Bottom) => true
    case _ => false
  }
  println(named.size)
  //named.foreach(println)

  val reasoner = new ElkReasonerFactory().createReasoner(ontology)
  val terms = uberonAxioms.collect { case ax: ConceptInclusion => ax }.flatMap(_.signature).collect { case e: AtomicConcept => e }
  val elkConceptInclusions = terms.flatMap(t => reasoner.getSubClasses(Class(t.id), false).getFlattened.asScala.map(sub => ConceptInclusion(AtomicConcept(sub.getIRI.toString), t))).filterNot(_.subclass == Bottom)
  val elkConceptInclusionsCount = elkConceptInclusions.size
  println(s"ELK: $elkConceptInclusionsCount")
  reasoner.dispose()

  //val ci = ConceptInclusion(AtomicConcept("http://example.org/uberon#basal_vein"), ExistentialRestriction(Role("http://example.org/uberon#develops_from"), AtomicConcept("http://example.org/uberon#ectoderm")))
  val ci = ConceptInclusion(AtomicConcept("http://example.org/uberon#telencephalon"), ExistentialRestriction(Role("http://example.org/uberon#develops_from"), AtomicConcept("http://example.org/uberon#anterior_neural_tube")))
  //val ci = ConceptInclusion(AtomicConcept("http://example.org/uberon#basal_vein"), AtomicConcept("http://example.org/uberon#anatomical_structure"))
  println(s"Contains: ${done.subs(ci)}")

  val missingFromWhelk = elkConceptInclusions -- named
  val missingFromElk = named -- elkConceptInclusions

  println(s"Missing from Whelk: ${missingFromWhelk.size}:")
  println(missingFromWhelk.take(20))
  println(s"Missing from Elk: ${missingFromElk.size}:")
  println(missingFromElk.take(20))

}
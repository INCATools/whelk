package org.geneontology.whelk

import java.io.File

import scala.collection.JavaConverters._

import org.phenoscape.scowl._
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

import BuiltIn.Bottom

object Test extends App {

  //val reasoner = Reasoner.prepare(Bridge.ontologyToAxioms(OWLManager.createOWLOntologyManager().loadOntology(IRI.create("http://purl.obolibrary.org/obo/pato.owl"))))
  val ontology = OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("uberon-trimmed.ofn")))
  val uberonAxioms = Bridge.ontologyToAxioms(ontology)
  //val goAxioms = Bridge.ontologyToAxioms(OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("../../Source/obo-asserted/go.owl"))))
  //val reasoner = Reasoner.prepare(Bridge.ontologyToAxioms(OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("skeletons.ofn")))))
  println("Start")
  val start = System.currentTimeMillis
  val done = Reasoner.assert(uberonAxioms)
  val stop = System.currentTimeMillis
  println(s"Reasoned in: ${stop - start} ms")

  //done.subs.foreach(println)
  println("================")

  val query = ConceptInclusion(
    Conjunction(
      AtomicConcept("http://purl.obolibrary.org/obo/UBERON_0001630"),
      ExistentialRestriction(Role("http://purl.obolibrary.org/obo/BFO_0000050"), AtomicConcept("http://purl.obolibrary.org/obo/UBERON_0000033"))),
    AtomicConcept("http://example.org/muscle_of_head"))
  val queryStart = System.currentTimeMillis
  val newDone = Reasoner.assert(Set(query), done)
  val queryStop = System.currentTimeMillis
  println(s"Classified query in ${queryStop - queryStart} ms")
  val subclasses = newDone.subs.filter(_.superclass == AtomicConcept("http://example.org/muscle_of_head")).map(_.subclass).collect { case x: AtomicConcept => x }
  //subclasses.foreach(println)

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

  //  println(done.hier(Role("http://purl.obolibrary.org/obo/BFO_0000050")))
  //  println(done.hier(Role("http://purl.obolibrary.org/obo/RO_0002202")))
  //  println("==================")
  //  println(done.hierComps(Role("http://purl.obolibrary.org/obo/BFO_0000050")))
  //  println(done.hierComps(Role("http://purl.obolibrary.org/obo/RO_0002202")))

}
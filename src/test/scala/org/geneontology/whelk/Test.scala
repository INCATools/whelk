package org.geneontology.whelk

import java.io.File

import scala.collection.JavaConverters._

import org.phenoscape.scowl._
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

import BuiltIn._

object Test extends App {

  def time[T](message: String)(operation: => T): T = {
    val start = System.currentTimeMillis
    val completed = operation
    val stop = System.currentTimeMillis
    println(s"$message: ${stop - start} ms")
    completed
  }

  //val reasoner = Reasoner.prepare(Bridge.ontologyToAxioms(OWLManager.createOWLOntologyManager().loadOntology(IRI.create("http://purl.obolibrary.org/obo/pato.owl"))))
  val ontology = OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("uberon-go-cl-ro.ofn")))
  val uberonAxioms = Bridge.ontologyToAxioms(ontology)
  //val goAxioms = Bridge.ontologyToAxioms(OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("../../Source/obo-asserted/go.owl"))))
  //val reasoner = Reasoner.prepare(Bridge.ontologyToAxioms(OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("skeletons.ofn")))))
  val gocam = OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File("586fc17a00001662.ofn")))
  val gocamAxioms = Bridge.ontologyToAxioms(gocam).collect { case ci: ConceptInclusion => ci }
  println(s"GO-CAM size: ${gocamAxioms.size}")

  println("Start")
  val done = time("Reasoned in:")(Reasoner.assert(uberonAxioms))

  //done.subs.foreach(println)
  println("================")

  val query = ConceptInclusion(
    Conjunction(
      AtomicConcept("http://purl.obolibrary.org/obo/UBERON_0001630"),
      ExistentialRestriction(Role("http://purl.obolibrary.org/obo/BFO_0000050"), AtomicConcept("http://purl.obolibrary.org/obo/UBERON_0000033"))),
    AtomicConcept("http://example.org/muscle_of_head"))

  val newDone = time("Classified query in")(Reasoner.assert(gocamAxioms, done))
  newDone.classAssertions.foreach(println)
  println

  val RegeneratingLimbFin = AtomicConcept("http://purl.obolibrary.org/obo/UBERON_2001269")
  val PectoralFin = AtomicConcept("http://purl.obolibrary.org/obo/UBERON_0000151")
  val CHEBIterm = AtomicConcept("http://purl.obolibrary.org/obo/CHEBI_33699")

  val taxonomy = time("Computed taxonomy in")(done.computeTaxonomy)

  println(taxonomy(RegeneratingLimbFin))
  println(taxonomy(PectoralFin))
  println(taxonomy(CHEBIterm))

  val indTypes = time("Computed direct types for individuals in")(newDone.individualsDirectTypes)
  indTypes.foreach(println)

  //val subclasses = newDone.subs.filter(_.superclass == AtomicConcept("http://example.org/muscle_of_head")).map(_.subclass).collect { case x: AtomicConcept => x }
  //subclasses.foreach(println)

  //done.subs.collect { case (ci @ ConceptInclusion(AtomicConcept(_), AtomicConcept(_))) => ci }.foreach(println)
  //println(done.concIncs.size)
  //println(done.subs.size - reasoner.concIncs.size)

  val doneSubs = done.subs
  println(doneSubs.size)
  //done.subs.foreach(println)
  val named = doneSubs.filter {
    case ConceptInclusion(sub: AtomicConcept, sup: AtomicConcept) if (sub != sup && sub != Bottom && sup != Top) => true
    case _ => false
  }
  println(named.size)
  //named.foreach(println)

  val reasoner = new ElkReasonerFactory().createReasoner(ontology)
  val terms = uberonAxioms.collect { case ax: ConceptInclusion => ax }.flatMap(_.signature).collect { case e: AtomicConcept => e }
  val elkConceptInclusions = terms.filterNot(_ == Top).flatMap(t => reasoner.getSubClasses(Class(t.id), false).getFlattened.asScala.map(sub => ConceptInclusion(AtomicConcept(sub.getIRI.toString), t))).filterNot(_.subclass == Bottom)
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
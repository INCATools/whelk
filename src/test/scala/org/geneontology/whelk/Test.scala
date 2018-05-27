package org.geneontology.whelk

import scala.collection.immutable.Queue
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

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

  val axioms = Set(
    ConceptInclusion(A, B),
    ConceptInclusion(B, C),
    ConceptInclusion(D, E),
    ConceptInclusion(B, ExistentialRestriction(R, D)),
    ConceptInclusion(ExistentialRestriction(R, E), F),
    ConceptInclusion(G, Conjunction(C, F)),
    ConceptInclusion(Conjunction(C, F), G),
    ConceptInclusion(D, ExistentialRestriction(S, H)),
    ConceptInclusion(H, ExistentialRestriction(S, I)),
    ConceptInclusion(ExistentialRestriction(S, I), H))

  val reasoner1 = Reasoner.empty.copy(
    todo = Queue.empty.enqueue(axioms),
    concIncs = axioms,
    negConjs = Set(Conjunction(C, F)),
    negExists = Set(ExistentialRestriction(R, E), ExistentialRestriction(S, I)),
    hier = Map(
      R -> Set(R, S),
      S -> Set(S)),
    roleComps = Map((S, S) -> Set(S)))

  val reasoner = Reasoner.prepare(Bridge.ontologyToAxioms(OWLManager.createOWLOntologyManager().loadOntology(IRI.create("http://purl.obolibrary.org/obo/uberon/ext.owl"))))

  val start = System.currentTimeMillis
  val done = Reasoner.computeClosure(reasoner)
  val stop = System.currentTimeMillis
  println(s"Reasoned in: ${stop - start} ms")
  done.subs.foreach(println)
  println("================")
  done.subs.collect { case (ci @ ConceptInclusion(AtomicConcept(_), AtomicConcept(_))) => ci }.foreach(println)

}
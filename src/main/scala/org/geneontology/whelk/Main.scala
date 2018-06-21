package org.geneontology.whelk

import java.io.File

import scala.collection.JavaConverters._

import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

import upickle.default._

import BuiltIn.Bottom
import java.io.FileWriter
import java.io.BufferedWriter

object Main extends App {

  val ontology = OWLManager.createOWLOntologyManager().loadOntology(IRI.create(args(0)))
  val axioms = Bridge.ontologyToAxioms(ontology)
  println("Start")
  val start = System.currentTimeMillis
  val done = Reasoner.assert(axioms)
  val stop = System.currentTimeMillis
  println(s"Reasoned in: ${stop - start} ms")

  val dumpStart = System.currentTimeMillis

  val writer = new BufferedWriter(new FileWriter(new File(args(1))))
  writeTo(done, writer)
  writer.close()

  val dumpStop = System.currentTimeMillis
  println(s"Dumped closure in ${dumpStop - dumpStart} ms")

}
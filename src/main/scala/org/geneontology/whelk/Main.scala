package org.geneontology.whelk

import java.io.File
import java.io.FileOutputStream
import java.io.FileInputStream

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

import boopickle.Default._
import java.nio.ByteBuffer

object Main extends App {

  val ontology = OWLManager.createOWLOntologyManager().loadOntology(IRI.create(args(0)))
  val axioms = Bridge.ontologyToAxioms(ontology)
  println("Start")
  val start = System.currentTimeMillis
  val done = Reasoner.assert(axioms)
  val stop = System.currentTimeMillis
  println(s"Reasoned in: ${stop - start} ms")

  val dumpStart = System.currentTimeMillis

  val buffer = Pickle.intoBytes(done)
  val channel = new FileOutputStream(new File(args(1)), false).getChannel
  channel.write(buffer)
  channel.close()

  val dumpStop = System.currentTimeMillis
  println(s"Dumped closure in ${dumpStop - dumpStart} ms")

  val fis = new FileInputStream(args(1))
  val inChannel = fis.getChannel
  val size = inChannel.size
  val inBuffer = ByteBuffer.allocate(size.toInt)
  inChannel.read(inBuffer)
  inBuffer.rewind()
  val unpickledReasoner = Unpickle[ReasonerState].fromBytes(inBuffer)
  inChannel.close()
  fis.close()
  println(s"Are they the same? ${done == unpickledReasoner}")

}
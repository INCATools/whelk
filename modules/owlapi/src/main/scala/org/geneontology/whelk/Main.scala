package org.geneontology.whelk

import java.io.File
import java.io.FileOutputStream
import java.io.FileInputStream
import java.util.UUID

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI

//import boopickle.Default._
import java.nio.ByteBuffer

object Main extends App {

  val startRead = System.currentTimeMillis()
  val ontology = OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File(args(0))))
  val stopRead = System.currentTimeMillis()
  println(s"Read ontology in: ${stopRead - startRead} ms")
  println(s"${ontology.getAxiomCount} OWL axioms")
  val axioms = Bridge.ontologyToAxioms(ontology)
  println(s"${axioms.size} Whelk axioms")
  println("Start")
  val start = System.currentTimeMillis
  val done = Reasoner.assert(axioms)
  val stop = System.currentTimeMillis
  println(s"Reasoned in: ${stop - start} ms")
  val startTax = System.currentTimeMillis()
  val taxonomy = done.computeTaxonomy
  val stopTax = System.currentTimeMillis()
  println(s"Computed taxonomy in ${stopTax - startTax} ms")

  val properties = done.hier.keys
  val classes = done.inits.collect { case c: AtomicConcept => c }.take(100)
  val expressions = for {
    property <- properties
    cls <- classes
  } yield ExistentialRestriction(property, cls)
  println(s"Start ${expressions.size} queries")
  val startQueries = System.currentTimeMillis()
  for (expression <- expressions) {
    val cls = AtomicConcept(s"urn:uuid:${UUID.randomUUID.toString}")
    val expressionDone = Reasoner.assert(Set(ConceptInclusion(cls, expression), ConceptInclusion(expression, cls)), done)
    val subclasses = expressionDone.closureSubsBySuperclass.getOrElse(cls, Set.empty) + BuiltIn.Bottom
    val minusEquivs = subclasses.diff(expressionDone.closureSubsBySubclass.getOrElse(cls, Set.empty))
    val properSubclasses = minusEquivs.collect { case ac @ AtomicConcept(_) => ac }
  }
  val stopQueries = System.currentTimeMillis()
  println(s"Queries done in: ${stopQueries - startQueries} ms")

  val model = OWLManager.createOWLOntologyManager().loadOntology(IRI.create(new File(args(1))))
  val modelAxioms = Bridge.ontologyToAxioms(model)
  val modelConceptInclusions = modelAxioms.collect { case ci: ConceptInclusion => ci }
  val startModel = System.currentTimeMillis
  println("Start models")
  for (i <- 0 to 300) {
    val modelDone = Reasoner.assert(modelConceptInclusions, done)
  }
  val stopModel = System.currentTimeMillis
  println(s"Models reasoned in: ${stopModel - startModel} ms")

  //val dumpStart = System.currentTimeMillis

  // this is needed to help the macro for some reason
  //  implicit val pickler: Pickler[Disjunction] = generatePickler[Disjunction]
  //
  //  val buffer = Pickle.intoBytes(done)
  //  val fos = new FileOutputStream(new File(args(1)), false)
  //  val channel = fos.getChannel
  //  channel.write(buffer)
  //  channel.close()
  //  fos.close()
  //
  //  val dumpStop = System.currentTimeMillis
  //  println(s"Dumped closure in ${dumpStop - dumpStart} ms")
  //
  //  val unpickleStart = System.currentTimeMillis
  //
  //  val fis = new FileInputStream(args(1))
  //  val inChannel = fis.getChannel
  //  val size = inChannel.size
  //  val inBuffer = ByteBuffer.allocate(size.toInt)
  //  inChannel.read(inBuffer)
  //  inBuffer.rewind()
  //  val unpickledReasoner = Unpickle[ReasonerState].fromBytes(inBuffer)
  //  inChannel.close()
  //  fis.close()
  //
  //  val unpickleStop = System.currentTimeMillis
  //  println(s"Unpickled closure in ${unpickleStop - unpickleStart} ms")
  //
  //  println(s"Are they the same? ${done == unpickledReasoner}")

}
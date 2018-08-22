package org.geneontology.whelk.owlapi

import java.util.Collections
import java.util.{ List => JList }
import java.util.{ Set => JSet }
import java.util.UUID

import scala.collection.JavaConverters._
import scala.collection.immutable.Queue

import org.geneontology.whelk.AtomicConcept
import org.geneontology.whelk.Bridge
import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.ConceptInclusion
import org.geneontology.whelk.Individual
import org.geneontology.whelk.Nominal
import org.geneontology.whelk.Reasoner
import org.geneontology.whelk.ReasonerState
import org.geneontology.whelk.Role
import org.geneontology.whelk.RoleAssertion
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.AxiomType
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClass
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLDataProperty
import org.semanticweb.owlapi.model.OWLDataPropertyExpression
import org.semanticweb.owlapi.model.OWLLiteral
import org.semanticweb.owlapi.model.OWLNamedIndividual
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLOntologyChange
import org.semanticweb.owlapi.model.OWLOntologyChangeListener
import org.semanticweb.owlapi.reasoner.BufferingMode
import org.semanticweb.owlapi.reasoner.FreshEntityPolicy
import org.semanticweb.owlapi.reasoner.IndividualNodeSetPolicy
import org.semanticweb.owlapi.reasoner.InferenceType
import org.semanticweb.owlapi.reasoner.Node
import org.semanticweb.owlapi.reasoner.NodeSet
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.reasoner.impl.NodeFactory
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet
import org.semanticweb.owlapi.util.Version

/**
 * WhelkOWLReasoner provides an OWL API OWLReasoner wrapper for Whelk.
 * It is not thread-safe for ontology changes, but should be able to
 * answer multiple queries in parallel. For better multithreaded
 * applications use the Whelk Scala API instead.
 */
class WhelkOWLReasoner(ontology: OWLOntology, bufferingMode: BufferingMode) extends OWLReasoner {

  private var whelk: ReasonerState = ReasonerState.empty

  private val factory = ontology.getOWLOntologyManager.getOWLDataFactory

  private var hasPendingChanges: Boolean = true
  private var pendingChanges: Queue[OWLOntologyChange] = Queue.empty
  private var pendingAxiomAdditions: Set[OWLAxiom] = Set.empty
  private var pendingAxiomRemovals: Set[OWLAxiom] = Set.empty

  private object ChangeListener extends OWLOntologyChangeListener {

    //TODO handle incremental changes from OWL API
    override def ontologiesChanged(changes: JList[_ <: OWLOntologyChange]): Unit = {
      hasPendingChanges = true
      for (change <- changes.asScala) {
        pendingChanges = pendingChanges.enqueue(change)
        if (change.isAxiomChange()) {
          pendingAxiomAdditions = pendingAxiomAdditions + change.getAxiom
          if (change.isAddAxiom()) {
            pendingAxiomAdditions += change.getAxiom
          } else if (change.isRemoveAxiom()) {
            pendingAxiomRemovals += change.getAxiom
          }
        }
      }
      if (bufferingMode == BufferingMode.NON_BUFFERING) flush()
    }
  }

  ontology.getOWLOntologyManager.addOntologyChangeListener(ChangeListener)

  flush()

  override def dispose(): Unit = {
    ontology.getOWLOntologyManager.removeOntologyChangeListener(ChangeListener)
  }

  override def flush(): Unit = {
    whelk = Reasoner.assert(Bridge.ontologyToAxioms(ontology))
  }

  override def getDifferentIndividuals(ind: OWLNamedIndividual): NodeSet[OWLNamedIndividual] =
    throw new UnsupportedOperationException("getDifferentIndividuals")

  override def getInstances(ce: OWLClassExpression, direct: Boolean): NodeSet[OWLNamedIndividual] = {
    val (concept, reasoner) = Bridge.convertExpression(ce) match {
      case Some(named @ AtomicConcept(_)) => (named, whelk)
      case Some(concept) =>
        val fresh = freshConcept
        (fresh, Reasoner.assert(Set(ConceptInclusion(concept, fresh)), whelk))
      case None => throw new UnsupportedOperationException(s"getInstances: $ce")
    }
    val subsumed = if (direct) {
      reasoner.directlySubsumes(concept)._2
    } else reasoner.closureSubsBySuperclass.getOrElse(concept, Set.empty)
    val individuals: Set[Node[OWLNamedIndividual]] = subsumed.collect {
      case Nominal(Individual(iri)) =>
        NodeFactory.getOWLNamedIndividualNode(NamedIndividual(iri))
    }
    new OWLNamedIndividualNodeSet(individuals.asJava)
  }

  override def getObjectPropertyValues(ind: OWLNamedIndividual, pe: OWLObjectPropertyExpression): NodeSet[OWLNamedIndividual] = {
    val Ind = Individual(ind.getIRI.toString)
    val values = pe match {
      case ObjectProperty(iri) =>
        val Prop = Role(iri.toString)
        whelk.roleAssertions.collect { case RoleAssertion(Prop, Ind, value) => value }
      case ObjectInverseOf(ObjectProperty(iri)) =>
        val Prop = Role(iri.toString)
        whelk.roleAssertions.collect { case RoleAssertion(Prop, value, Ind) => value }
    }
    val individuals: Set[Node[OWLNamedIndividual]] = values.map(v => NamedIndividual(v.id)).map(NodeFactory.getOWLNamedIndividualNode) //FIXME same individuals should be grouped into nodes
    new OWLNamedIndividualNodeSet(individuals.asJava)
  }

  override def getDataPropertyValues(ind: OWLNamedIndividual, dp: OWLDataProperty): JSet[OWLLiteral] =
    throw new UnsupportedOperationException("getDataPropertyValues")

  override def getReasonerName(): String = "Whelk"

  override def getReasonerVersion(): Version = return new Version(0, 0, 0, 0); //FIXME

  override def getRootOntology(): OWLOntology = ontology

  override def getSameIndividuals(ind: OWLNamedIndividual): Node[OWLNamedIndividual] = throw new UnsupportedOperationException("getSameIndividuals")

  override def getSubClasses(ce: OWLClassExpression, direct: Boolean): NodeSet[OWLClass] = {
    val (concept, reasoner) = Bridge.convertExpression(ce) match {
      case Some(named @ AtomicConcept(_)) => (named, whelk)
      case Some(concept) =>
        val fresh = freshConcept
        (fresh, Reasoner.assert(Set(ConceptInclusion(fresh, concept), ConceptInclusion(concept, fresh)), whelk))
      case None => throw new UnsupportedOperationException(s"getSubClasses: $ce")
    }
    val subsumed: Set[AtomicConcept] = if (direct) {
      reasoner.directlySubsumes(concept)._2
    } else {
      val subclasses = reasoner.closureSubsBySuperclass.getOrElse(concept, Set.empty) + Bottom
      val minusEquivs = subclasses.diff(reasoner.closureSubsBySubclass.getOrElse(concept, Set.empty))
      minusEquivs.collect { case ac @ AtomicConcept(_) => ac }
    }
    val subsumedClasses = subsumed.map { case AtomicConcept(iri) => Class(iri) }
    val classNodes: Set[Node[OWLClass]] = subsumedClasses.map(NodeFactory.getOWLClassNode) //FIXME equivalent classes should be grouped into nodes
    new OWLClassNodeSet(classNodes.asJava)
  }

  override def getSubDataProperties(dp: OWLDataProperty, direct: Boolean): NodeSet[OWLDataProperty] = throw new UnsupportedOperationException("getSubDataProperties")

  override def getSubObjectProperties(ope: OWLObjectPropertyExpression, direct: Boolean): NodeSet[OWLObjectPropertyExpression] = throw new UnsupportedOperationException("getSubObjectProperties")

  override def getSuperClasses(ce: OWLClassExpression, direct: Boolean): NodeSet[OWLClass] = {
    val (concept, reasoner) = Bridge.convertExpression(ce) match {
      case Some(named @ AtomicConcept(_)) => (named, whelk)
      case Some(concept) =>
        val fresh = freshConcept
        (fresh, Reasoner.assert(Set(ConceptInclusion(fresh, concept), ConceptInclusion(concept, fresh)), whelk))
      case None => throw new UnsupportedOperationException(s"getSuperClasses: $ce")
    }
    val subsumers: Set[AtomicConcept] = if (direct) {
      reasoner.directlySubsumedBy(concept)._2
    } else {
      val superclasses = reasoner.closureSubsBySubclass.getOrElse(concept, Set.empty) + Top
      val minusEquivs = superclasses.diff(reasoner.closureSubsBySuperclass.getOrElse(concept, Set.empty))
      minusEquivs.collect { case ac @ AtomicConcept(_) => ac }
    }
    val subsumerClasses = subsumers.map { case AtomicConcept(iri) => Class(iri) }
    val classNodes: Set[Node[OWLClass]] = subsumerClasses.map(NodeFactory.getOWLClassNode) //FIXME equivalent classes should be grouped into nodes
    new OWLClassNodeSet(classNodes.asJava)
  }

  override def getSuperDataProperties(dp: OWLDataProperty, direct: Boolean): NodeSet[OWLDataProperty] = throw new UnsupportedOperationException("getSuperDataProperties")

  override def getSuperObjectProperties(ope: OWLObjectPropertyExpression, direct: Boolean): NodeSet[OWLObjectPropertyExpression] = throw new UnsupportedOperationException("getSuperObjectProperties")

  override def getTimeOut(): Long = Long.MaxValue //TODO support cancellation

  override def getTopClassNode(): Node[OWLClass] = {
    val topEquivalents = whelk.closureSubsBySubclass.getOrElse(Top, Set.empty) + Top
    val classes = topEquivalents.collect { case AtomicConcept(iri) => Class(iri) }
    NodeFactory.getOWLClassNode(classes.asJava)
  }

  override def getTopDataPropertyNode(): Node[OWLDataProperty] = throw new UnsupportedOperationException("getTopDataPropertyNode")

  override def getTopObjectPropertyNode(): Node[OWLObjectPropertyExpression] = throw new UnsupportedOperationException("getTopObjectPropertyNode")

  override def getBottomClassNode(): Node[OWLClass] = {
    val bottomEquivalents = whelk.closureSubsBySuperclass.getOrElse(Bottom, Set.empty) + Bottom
    val classes = bottomEquivalents.collect { case AtomicConcept(iri) => Class(iri) }
    NodeFactory.getOWLClassNode(classes.asJava)
  }

  override def getBottomDataPropertyNode(): Node[OWLDataProperty] = throw new UnsupportedOperationException("getBottomDataPropertyNode")

  override def getBottomObjectPropertyNode(): Node[OWLObjectPropertyExpression] = throw new UnsupportedOperationException("getBottomObjectPropertyNode")

  override def getBufferingMode(): BufferingMode = bufferingMode

  override def getTypes(ind: OWLNamedIndividual, direct: Boolean): NodeSet[OWLClass] = {
    val individual = Nominal(Individual(ind.getIRI.toString))
    val concepts = if (direct) whelk.directlySubsumedBy(individual)._2
    else whelk.closureSubsBySubclass.getOrElse(individual, Set.empty).collect { case ac: AtomicConcept => ac } + Top
    val classes = concepts.map { case AtomicConcept(iri) => Class(iri) }
    val classNodes: Set[Node[OWLClass]] = classes.map(NodeFactory.getOWLClassNode) //FIXME equivalent classes should be grouped into nodes
    new OWLClassNodeSet(classNodes.asJava)
  }

  override def interrupt(): Unit = ()

  override def isConsistent(): Boolean = whelk.closureSubsBySuperclass(Bottom).forall {
    case n: Nominal => false
    case _          => true
  }

  override def isEntailed(axiom: OWLAxiom): Boolean = throw new UnsupportedOperationException("isEntailed") //TODO

  override def isEntailed(axioms: JSet[_ <: OWLAxiom]): Boolean = throw new UnsupportedOperationException("isEntailed") //TODO

  override def precomputeInferences(infType: InferenceType*): Unit = ()

  override def getDataPropertyDomains(arg0: OWLDataProperty, arg1: Boolean): NodeSet[OWLClass] = throw new UnsupportedOperationException("getDataPropertyDomains")

  override def getDisjointClasses(arg0: OWLClassExpression): NodeSet[OWLClass] = throw new UnsupportedOperationException("getDisjointClasses")

  override def getDisjointDataProperties(arg0: OWLDataPropertyExpression): NodeSet[OWLDataProperty] = throw new UnsupportedOperationException("getDisjointDataProperties")

  override def getDisjointObjectProperties(arg0: OWLObjectPropertyExpression): NodeSet[OWLObjectPropertyExpression] = throw new UnsupportedOperationException("getDisjointObjectProperties")

  override def getEquivalentClasses(ce: OWLClassExpression): Node[OWLClass] = {
    val (concept, reasoner) = Bridge.convertExpression(ce) match {
      case Some(named @ AtomicConcept(_)) => (named, whelk)
      case Some(concept) =>
        val fresh = freshConcept
        (fresh, Reasoner.assert(Set(ConceptInclusion(fresh, concept), ConceptInclusion(concept, fresh)), whelk))
      case None => throw new UnsupportedOperationException(s"getEquivalentClasses: $ce")
    }
    val equivClasses = (reasoner.closureSubsBySubclass.getOrElse(concept, Set.empty).intersect(reasoner.closureSubsBySuperclass.getOrElse(concept, Set.empty)) - concept).collect { case AtomicConcept(iri) => Class(iri) }
    NodeFactory.getOWLClassNode(equivClasses.asJava)
  }

  override def getEquivalentDataProperties(arg0: OWLDataProperty): Node[OWLDataProperty] = throw new UnsupportedOperationException("getEquivalentDataProperties")

  override def getEquivalentObjectProperties(arg0: OWLObjectPropertyExpression): Node[OWLObjectPropertyExpression] = throw new UnsupportedOperationException("getEquivalentObjectProperties")

  override def getFreshEntityPolicy(): FreshEntityPolicy = FreshEntityPolicy.ALLOW

  override def getIndividualNodeSetPolicy(): IndividualNodeSetPolicy = IndividualNodeSetPolicy.BY_NAME //FIXME change if sameAs is supported

  override def getInverseObjectProperties(ope: OWLObjectPropertyExpression): Node[OWLObjectPropertyExpression] = throw new UnsupportedOperationException("getInverseObjectProperties")

  override def getObjectPropertyDomains(ope: OWLObjectPropertyExpression, direct: Boolean): NodeSet[OWLClass] = throw new UnsupportedOperationException("getObjectPropertyDomains")

  override def getObjectPropertyRanges(ope: OWLObjectPropertyExpression, direct: Boolean): NodeSet[OWLClass] = throw new UnsupportedOperationException("getObjectPropertyRanges")

  override def getPendingAxiomAdditions(): JSet[OWLAxiom] = pendingAxiomAdditions.asJava

  override def getPendingAxiomRemovals(): JSet[OWLAxiom] = pendingAxiomRemovals.asJava

  override def getPendingChanges(): JList[OWLOntologyChange] = pendingChanges.asJava

  override def getPrecomputableInferenceTypes(): JSet[InferenceType] = Collections.emptySet()

  override def isEntailmentCheckingSupported(axiomType: AxiomType[_]): Boolean = false //FIXME should be able to handle some

  override def isPrecomputed(inferenceType: InferenceType): Boolean = false

  override def isSatisfiable(ce: OWLClassExpression): Boolean = {
    val (concept, reasoner) = Bridge.convertExpression(ce) match {
      case Some(named @ AtomicConcept(_)) => (named, whelk)
      case Some(concept) =>
        val fresh = freshConcept
        (fresh, Reasoner.assert(Set(ConceptInclusion(fresh, concept)), whelk))
      case None => throw new UnsupportedOperationException(s"getEquivalentClasses: $ce")
    }
    !reasoner.closureSubsBySubclass(concept)(Bottom)
  }

  override def getUnsatisfiableClasses(): Node[OWLClass] = getBottomClassNode()

  private def freshConcept: AtomicConcept = AtomicConcept(s"urn:uuid:${UUID.randomUUID.toString}")

}
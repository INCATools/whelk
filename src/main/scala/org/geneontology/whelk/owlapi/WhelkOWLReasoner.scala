package org.geneontology.whelk.owlapi

import java.util.{UUID, List => JList, Set => JSet}

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.{AtomicConcept, Bridge, ConceptInclusion, Nominal, Reasoner, ReasonerState, Role, RoleAssertion, Individual => WhelkIndividual}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner._
import org.semanticweb.owlapi.reasoner.impl.{NodeFactory, OWLClassNodeSet, OWLNamedIndividualNode, OWLNamedIndividualNodeSet}
import org.semanticweb.owlapi.util.Version

import scala.collection.JavaConverters._
import scala.collection.immutable.Queue

/**
 * WhelkOWLReasoner provides an OWL API OWLReasoner wrapper for Whelk.
 * It is not thread-safe for ontology changes, but should be able to
 * answer multiple queries in parallel. For better multithreaded
 * applications use the Whelk Scala API instead.
 */
class WhelkOWLReasoner(ontology: OWLOntology, bufferingMode: BufferingMode) extends OWLReasoner {

  private var whelk: ReasonerState = ReasonerState.empty
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
          if (change.isAddAxiom) {
            pendingAxiomAdditions += change.getAxiom
          } else if (change.isRemoveAxiom) {
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
    pendingChanges = Queue.empty
    pendingAxiomAdditions = Set.empty
    pendingAxiomRemovals = Set.empty
    hasPendingChanges = false
  }

  override def getDifferentIndividuals(ind: OWLNamedIndividual): NodeSet[OWLNamedIndividual] =
    throw new UnsupportedOperationException("getDifferentIndividuals")

  override def getInstances(ce: OWLClassExpression, direct: Boolean): NodeSet[OWLNamedIndividual] = {
    val (concept, reasoner) = Bridge.convertExpression(ce) match {
      case Some(named @ AtomicConcept(_)) => (named, whelk)
      case Some(expression)               =>
        val fresh = freshConcept()
        (fresh, Reasoner.assert(Set(ConceptInclusion(expression, fresh)), whelk))
      case None                           => throw new UnsupportedOperationException(s"getInstances: $ce")
    }
    val subsumed = if (direct) {
      reasoner.directlySubsumes(concept)._2
    } else reasoner.closureSubsBySuperclass.getOrElse(concept, Set.empty)
    val individuals: Set[Node[OWLNamedIndividual]] = subsumed.collect {
      case Nominal(WhelkIndividual(iri)) =>
        NodeFactory.getOWLNamedIndividualNode(NamedIndividual(iri))
    }
    new OWLNamedIndividualNodeSet(individuals.asJava)
  }

  override def getObjectPropertyValues(ind: OWLNamedIndividual, pe: OWLObjectPropertyExpression): NodeSet[OWLNamedIndividual] = {
    val Ind = WhelkIndividual(ind.getIRI.toString)
    val values = pe match {
      case ObjectProperty(iri)                  =>
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

  override def getReasonerVersion(): Version = new Version(0, 0, 0, 0) //FIXME

  override def getRootOntology(): OWLOntology = ontology

  override def getSameIndividuals(ind: OWLNamedIndividual): Node[OWLNamedIndividual] = new OWLNamedIndividualNode(ind) //FIXME implement correctly

  override def getSubClasses(ce: OWLClassExpression, direct: Boolean): NodeSet[OWLClass] = {
    val (concept, reasoner) = Bridge.convertExpression(ce) match {
      case Some(named @ AtomicConcept(_)) => (named, whelk)
      case Some(expression)               =>
        val fresh = freshConcept()
        (fresh, Reasoner.assert(Set(ConceptInclusion(fresh, expression), ConceptInclusion(expression, fresh)), whelk))
      case None                           => throw new UnsupportedOperationException(s"getSubClasses: $ce")
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
      case Some(expression)               =>
        val fresh = freshConcept()
        (fresh, Reasoner.assert(Set(ConceptInclusion(fresh, expression), ConceptInclusion(expression, fresh)), whelk))
      case None                           => throw new UnsupportedOperationException(s"getSuperClasses: $ce")
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

  override def getTopObjectPropertyNode(): Node[OWLObjectPropertyExpression] = NodeFactory.getOWLObjectPropertyTopNode

  override def getBottomClassNode(): Node[OWLClass] = {
    val bottomEquivalents = whelk.closureSubsBySuperclass.getOrElse(Bottom, Set.empty) + Bottom
    val classes = bottomEquivalents.collect { case AtomicConcept(iri) => Class(iri) }
    NodeFactory.getOWLClassNode(classes.asJava)
  }

  override def getBottomDataPropertyNode(): Node[OWLDataProperty] = throw new UnsupportedOperationException("getBottomDataPropertyNode")

  override def getBottomObjectPropertyNode(): Node[OWLObjectPropertyExpression] = NodeFactory.getOWLObjectPropertyBottomNode

  override def getBufferingMode(): BufferingMode = bufferingMode

  override def getTypes(ind: OWLNamedIndividual, direct: Boolean): NodeSet[OWLClass] = {
    val individual = Nominal(WhelkIndividual(ind.getIRI.toString))
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
      case Some(expression)               =>
        val fresh = freshConcept()
        (fresh, Reasoner.assert(Set(ConceptInclusion(fresh, expression), ConceptInclusion(expression, fresh)), whelk))
      case None                           => throw new UnsupportedOperationException(s"getEquivalentClasses: $ce")
    }
    val equivClasses = (reasoner.closureSubsBySubclass.getOrElse(concept, Set.empty).intersect(reasoner.closureSubsBySuperclass.getOrElse(concept, Set.empty)) - concept).collect { case AtomicConcept(iri) => Class(iri) }
    NodeFactory.getOWLClassNode(equivClasses.asJava)
  }

  override def getEquivalentDataProperties(arg0: OWLDataProperty): Node[OWLDataProperty] = throw new UnsupportedOperationException("getEquivalentDataProperties")

  override def getEquivalentObjectProperties(pe: OWLObjectPropertyExpression): Node[OWLObjectPropertyExpression] = NodeFactory.getOWLObjectPropertyNode(pe) //FIXME ?

  override def getFreshEntityPolicy(): FreshEntityPolicy = FreshEntityPolicy.ALLOW

  override def getIndividualNodeSetPolicy(): IndividualNodeSetPolicy = IndividualNodeSetPolicy.BY_NAME //FIXME change if sameAs is supported

  override def getInverseObjectProperties(ope: OWLObjectPropertyExpression): Node[OWLObjectPropertyExpression] = throw new UnsupportedOperationException("getInverseObjectProperties")

  override def getObjectPropertyDomains(ope: OWLObjectPropertyExpression, direct: Boolean): NodeSet[OWLClass] = throw new UnsupportedOperationException("getObjectPropertyDomains")

  override def getObjectPropertyRanges(ope: OWLObjectPropertyExpression, direct: Boolean): NodeSet[OWLClass] = throw new UnsupportedOperationException("getObjectPropertyRanges")

  override def getPendingAxiomAdditions(): JSet[OWLAxiom] = pendingAxiomAdditions.asJava

  override def getPendingAxiomRemovals(): JSet[OWLAxiom] = pendingAxiomRemovals.asJava

  override def getPendingChanges(): JList[OWLOntologyChange] = pendingChanges.asJava

  override def getPrecomputableInferenceTypes(): JSet[InferenceType] =
    Set(InferenceType.CLASS_ASSERTIONS, InferenceType.CLASS_HIERARCHY, InferenceType.OBJECT_PROPERTY_ASSERTIONS).asJava

  override def isEntailmentCheckingSupported(axiomType: AxiomType[_]): Boolean = false //FIXME should be able to handle some

  override def isPrecomputed(inferenceType: InferenceType): Boolean = true

  override def isSatisfiable(ce: OWLClassExpression): Boolean = {
    // First we handle the special case that a class assertion or object property assertion is being checked by the OWL API explanation tool.
    // We need to include inferences made by the RL engine.
    val maybeSatisfiable = ce match {
      case ObjectIntersectionOf(operands) if operands.size == 2 =>
        operands.toList match {
          case ObjectOneOf(individuals) :: ObjectComplementOf(expression) :: Nil if individuals.size == 1 => Some(!(getInstances(expression, false).getFlattened.contains(individuals.head)))
          case ObjectComplementOf(expression) :: ObjectOneOf(individuals) :: Nil if individuals.size == 1 => Some(!(getInstances(expression, false).getFlattened.contains(individuals.head)))
          case _                                                                                          => None
        }
      case _                                                    => None
    }
    maybeSatisfiable.getOrElse {
      // This is the "normal" path to check EL inferences.
      val (concept, reasoner) = Bridge.convertExpression(ce) match {
        case Some(named @ AtomicConcept(_)) => (named, whelk)
        case Some(expression)               =>
          val fresh = freshConcept()
          (fresh, Reasoner.assert(Set(ConceptInclusion(fresh, expression)), whelk))
        case None                           => throw new UnsupportedOperationException(s"getEquivalentClasses: $ce")
      }
      !reasoner.closureSubsBySubclass.getOrElse(concept, Set.empty)(Bottom)
    }
  }

  override def getUnsatisfiableClasses(): Node[OWLClass] = getBottomClassNode()

  private def freshConcept(): AtomicConcept = AtomicConcept(s"urn:uuid:${UUID.randomUUID.toString}")

  def getAllObjectPropertyValues(ind: OWLNamedIndividual): JSet[OWLObjectPropertyAssertionAxiom] = {
    val Ind = WhelkIndividual(ind.getIRI.toString)
    whelk.roleAssertions.collect { case RoleAssertion(prop, Ind, value) => ObjectPropertyAssertion(ObjectProperty(prop.id), ind, NamedIndividual(value.id)) }.asJava
  }

}
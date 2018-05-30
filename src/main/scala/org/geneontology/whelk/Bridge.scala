package org.geneontology.whelk

import scala.collection.JavaConverters._

import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.parameters.Imports

object Bridge {

  def ontologyToAxioms(ont: OWLOntology): Set[Axiom] = ont.getAxioms(Imports.INCLUDED).asScala.flatMap(convertAxiom).toSet

  def convertAxiom(owlAxiom: OWLAxiom): Set[Axiom] = owlAxiom match {
    case SubClassOf(_, subclass, superclass) => (convertExpression(subclass), convertExpression(superclass)) match {
      case (Some(subConcept), Some(superConcept)) => Set(ConceptInclusion(subConcept, superConcept))
      case _                                      => Set.empty
    }
    case EquivalentClasses(_, operands) if operands.size == 2 => //FIXME handle >2
      val converted = operands.map(convertExpression).toSeq
      (converted(0), converted(1)) match {
        case (Some(left), Some(right)) => Set(ConceptInclusion(left, right), ConceptInclusion(right, left))
        case _                         => Set.empty
      }
    case DisjointClasses(_, operands) if operands.size == 2 => //FIXME handle >2
      val converted = operands.map(convertExpression).toSeq
      (converted(0), converted(1)) match {
        case (Some(left), Some(right)) => Set(ConceptInclusion(Conjunction(left, right), Bottom))
        case _                         => Set.empty
      }
    case SubObjectPropertyOf(_, ObjectProperty(subproperty), ObjectProperty(superproperty)) =>
      Set(RoleInclusion(Role(subproperty.toString), Role(superproperty.toString)))
    case SubObjectPropertyChainOf(_, ObjectProperty(first) :: ObjectProperty(second) :: Nil, ObjectProperty(superproperty)) => //FIXME handle >2
      Set(RoleComposition(Role(first.toString), Role(second.toString), Role(superproperty.toString)))
    case TransitiveObjectProperty(_, ObjectProperty(property)) =>
      Set(RoleComposition(Role(property.toString), Role(property.toString), Role(property.toString)))
    case other =>
      //println(s"Not supported: $other")
      Set.empty
  }

  def convertExpression(expression: OWLClassExpression): Option[Concept] = expression match {
    case OWLThing => Some(Top)
    case OWLNothing => Some(Bottom)
    case Class(iri) => Some(AtomicConcept(iri.toString))
    case ObjectSomeValuesFrom(ObjectProperty(prop), filler) => convertExpression(filler).map(ExistentialRestriction(Role(prop.toString), _))
    case ObjectIntersectionOf(operands) if operands.size == 2 => //FIXME convert >2 to binary
      val converted = operands.toSeq.map(convertExpression)
      (converted(0), converted(1)) match {
        case (Some(left), Some(right)) => Some(Conjunction(left, right))
        case _                         => None
      }
    case other =>
      //println(s"Not supported: $other")
      None
  }

}
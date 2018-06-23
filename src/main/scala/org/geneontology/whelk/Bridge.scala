package org.geneontology.whelk

import scala.collection.JavaConverters._

import org.phenoscape.scowl._
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.parameters.Imports

import BuiltIn._

object Bridge {

  def ontologyToAxioms(ont: OWLOntology): Set[Axiom] = ont.getAxioms(Imports.INCLUDED).asScala.flatMap(convertAxiom).toSet

  def convertAxiom(owlAxiom: OWLAxiom): Set[Axiom] = owlAxiom match {
    case SubClassOf(_, subclass, superclass) => (convertExpression(subclass), convertExpression(superclass)) match {
      case (Some(subConcept), Some(superConcept)) => Set(ConceptInclusion(subConcept, superConcept))
      case _                                      => Set.empty
    }
    case EquivalentClasses(_, operands) =>
      val converted = operands.map(convertExpression).toList.collect { case Some(concept) => concept }
      converted.combinations(2).flatMap {
        case first :: second :: Nil => Set(ConceptInclusion(first, second), ConceptInclusion(second, first))
        case _                      => ??? //impossible
      }.toSet
    case DisjointClasses(_, operands) if operands.size == 2 => //FIXME handle >2
      val converted = operands.map(convertExpression).toList.collect { case Some(concept) => concept }
      converted.combinations(2).flatMap {
        case first :: second :: Nil => Set(ConceptInclusion(Conjunction(first, second), Bottom))
        case _                      => ??? //impossible
      }.toSet
    case ClassAssertion(_, cls, NamedIndividual(iri)) => convertExpression(cls).map(concept =>
      ConceptInclusion(Nominal(Individual(iri.toString)), concept)).toSet
    case ObjectPropertyAssertion(_, ObjectProperty(prop), NamedIndividual(subj), NamedIndividual(obj)) =>
      Set(ConceptInclusion(Nominal(Individual(subj.toString)), ExistentialRestriction(Role(prop.toString), Nominal(Individual(obj.toString)))))
    case ObjectPropertyAssertion(_, ObjectInverseOf(ObjectProperty(prop)), NamedIndividual(obj), NamedIndividual(subj)) =>
      Set(ConceptInclusion(Nominal(Individual(subj.toString)), ExistentialRestriction(Role(prop.toString), Nominal(Individual(obj.toString)))))
    case SubObjectPropertyOf(_, ObjectProperty(subproperty), ObjectProperty(superproperty)) =>
      Set(RoleInclusion(Role(subproperty.toString), Role(superproperty.toString)))
    case SubObjectPropertyChainOf(_, ObjectProperty(first) :: ObjectProperty(second) :: Nil, ObjectProperty(superproperty)) => //FIXME handle >2
      Set(RoleComposition(Role(first.toString), Role(second.toString), Role(superproperty.toString)))
    case TransitiveObjectProperty(_, ObjectProperty(property)) =>
      Set(RoleComposition(Role(property.toString), Role(property.toString), Role(property.toString)))
    case ObjectPropertyDomain(_, ObjectProperty(property), ce) => convertExpression(ce).map(concept =>
      //TODO Is there a faster way to implement Domain?
      ConceptInclusion(ExistentialRestriction(Role(property.toString), Top), concept)).toSet
    case other =>
      //println(s"Not supported: $other")
      Set.empty
  }

  def convertExpression(expression: OWLClassExpression): Option[Concept] = {
    import scalaz._
    import Scalaz._
    import org.geneontology.whelk.Disjunction
    expression match {
      case OWLThing => Some(Top)
      case OWLNothing => Some(Bottom)
      case Class(iri) => Some(AtomicConcept(iri.toString))
      case ObjectSomeValuesFrom(ObjectProperty(prop), filler) => convertExpression(filler).map(ExistentialRestriction(Role(prop.toString), _))
      case ObjectIntersectionOf(operands) if operands.size >= 2 =>
        def convert(items: List[Concept]): Concept = items match {
          case first :: second :: Nil  => Conjunction(first, second)
          case first :: second :: rest => Conjunction(first, convert(second :: rest))
          case first :: Nil            => first
        }
        operands.toList.map(convertExpression).sequence.map(convert)
      case ObjectUnionOf(operands) =>
        operands.toList.map(convertExpression).sequence.map(_.toSet).map(Disjunction(_))
      case ObjectComplementOf(concept) => convertExpression(concept).map(Complement(_))
      case other =>
        //println(s"Not supported: $other")
        None
    }
  }

}
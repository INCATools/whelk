package org.geneontology.whelk

import scala.collection.JavaConverters._

import org.phenoscape.scowl._
import org.phenoscape.scowl.ofn.SWRL
import org.semanticweb.owlapi.model.OWLAxiom
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.parameters.Imports
import org.geneontology.whelk.owlapi.SWRLUtil
import org.geneontology.whelk.{ Variable => WVariable }
import BuiltIn._
import org.semanticweb.owlapi.model.SWRLAtom
import org.semanticweb.owlapi.model.SWRLIArgument

object Bridge {

  object swrl extends SWRL
  import swrl._
  import SWRLUtil._
  import SWRLUtil.Variable
  import SWRLUtil.ClassAtom
  import SWRLUtil.ObjectPropertyAtom

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
      val sub = Role(subproperty.toString)
      val sup = Role(superproperty.toString)
      Set(
        RoleInclusion(sub, sup),
        Rule(List(RoleAtom(sub, WVariable("x1"), WVariable("x2"))), List(RoleAtom(sup, WVariable("x1"), WVariable("x2")))))
    case SubObjectPropertyChainOf(_, ObjectProperty(first) :: ObjectProperty(second) :: Nil, ObjectProperty(superproperty)) => //FIXME handle >2
      val supRole = Role(superproperty.toString)
      def makeSubject(level: Int): WVariable = if (level == 0) WVariable("x") else WVariable(s"x$level")
      val subprops = List(first, second) //for future handling of size >2
      val start = makeSubject(0)
      val end = makeSubject(subprops.size)
      val atoms = for {
        (subprop, index) <- subprops.zipWithIndex
        subrole = Role(subprop.toString)
      } yield RoleAtom(subrole, makeSubject(index), makeSubject(index + 1))
      Set(
        RoleComposition(Role(first.toString), Role(second.toString), supRole),
        Rule(body = atoms, head = List(RoleAtom(supRole, start, end))))
    case TransitiveObjectProperty(_, ObjectProperty(property)) =>
      val role = Role(property.toString)
      Set(
        RoleComposition(role, role, role),
        Rule(body = List(RoleAtom(role, WVariable("x1"), WVariable("x2")), RoleAtom(role, WVariable("x2"), WVariable("x3"))), head = List(RoleAtom(role, WVariable("x1"), WVariable("x3")))))
    case ObjectPropertyDomain(_, ObjectProperty(property), ce) => convertExpression(ce).map(concept =>
      //TODO Is there a faster way to implement Domain?
      ConceptInclusion(ExistentialRestriction(Role(property.toString), Top), concept)).toSet
    case ObjectPropertyRange(_, ObjectProperty(property), ce) => convertExpression(ce).map(concept =>
      //TODO only supporting in rules for now
      Rule(body = List(RoleAtom(Role(property.toString), WVariable("x1"), WVariable("x2"))), head = List(ConceptAtom(concept, WVariable("x2"))))).toSet
    case InverseObjectProperties(_, ObjectProperty(p), ObjectProperty(q)) =>
      val (roleP, roleQ) = (Role(p.toString), Role(q.toString))
      val (x1, x2) = (WVariable("x1"), WVariable("x2"))
      Set(
        Rule(body = List(RoleAtom(roleP, x1, x2)), head = List(RoleAtom(roleQ, x2, x1))),
        Rule(body = List(RoleAtom(roleQ, x1, x2)), head = List(RoleAtom(roleP, x2, x1))))
    case DLSafeRule(_, body, head) => (for {
      bodyAtoms <- convertAtomSet(body)
      headAtoms <- convertAtomSet(head)
    } yield Rule(bodyAtoms, headAtoms)).toSet
    case other =>
      //println(s"Not supported: $other")
      Set.empty
  }

  //TODO ObjectOneOf
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
      case ObjectComplementOf(concept)                                => convertExpression(concept).map(Complement(_))
      case ObjectOneOf(individuals) if individuals.size == 1          => individuals.collect { case NamedIndividual(iri) => Nominal(Individual(iri.toString)) }.headOption
      case ObjectHasValue(ObjectProperty(prop), NamedIndividual(ind)) => Some(ExistentialRestriction(Role(prop.toString), Nominal(Individual(ind.toString))))
      case other =>
        //println(s"Not supported: $other")
        None
    }
  }

  def convertAtomSet(atoms: Set[SWRLAtom]): Option[List[RuleAtom]] = {
    import scalaz._
    import Scalaz._
    atoms.toList.map(convertRuleAtom).sequence
  }

  def convertRuleAtom(atom: SWRLAtom): Option[RuleAtom] = atom match {
    case ClassAtom(ce, arg) => for {
      concept <- convertExpression(ce)
      argument <- convertAtomArg(arg)
    } yield ConceptAtom(concept, argument)
    case ObjectPropertyAtom(ObjectProperty(iri), subj, obj) => for {
      subject <- convertAtomArg(subj)
      target <- convertAtomArg(obj)
    } yield RoleAtom(Role(iri.toString), subject, target)
    case ObjectPropertyAtom(ObjectInverseOf(prop @ ObjectProperty(_)), subj, obj) => convertRuleAtom(ObjectPropertyAtom(prop, obj, subj))
    case _ => None
  }

  def convertAtomArg(arg: SWRLIArgument): Option[IndividualArgument] = arg match {
    case IndividualArg(NamedIndividual(iri)) => Some(Individual(iri.toString))
    case Variable(iri)                       => Some(WVariable(iri.toString))
    case _                                   => None
  }

}
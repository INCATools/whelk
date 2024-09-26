package org.geneontology.whelk

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.Role.CompositionRolePrefix
import org.geneontology.whelk.owlapi.SWRLUtil
import org.geneontology.whelk.{Individual => WIndividual, Variable => WVariable}
import org.phenoscape.scowl._
import org.phenoscape.scowl.ofn.SWRL
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

object Bridge {

  object swrl extends SWRL

  import SWRLUtil.{ClassAtom, ObjectPropertyAtom, Variable, _}
  import swrl._

  def ontologyToAxioms(ont: OWLOntology): Set[Axiom] = ont.getAxioms(Imports.INCLUDED).asScala.flatMap(convertAxiom).toSet

  def convertAxiom(owlAxiom: OWLAxiom): Set[Axiom] = owlAxiom match {
    case SubClassOf(_, subclass, superclass)                                                                                                   => (convertExpression(subclass), convertExpression(superclass)) match {
      case (Some(subConcept), Some(superConcept)) => Set(ConceptInclusion(subConcept, superConcept))
      case _                                      => Set.empty
    }
    case EquivalentClasses(_, operands)                                                                                                        =>
      val converted = operands.map(convertExpression).toList.collect { case Some(concept) => concept }
      converted.combinations(2).flatMap {
        case first :: second :: Nil => Set(ConceptInclusion(first, second), ConceptInclusion(second, first))
        case _                      => ??? //impossible
      }.toSet
    case DisjointClasses(_, operands)                                                                                                          =>
      val converted = operands.map(convertExpression).toList.collect { case Some(concept) => concept }
      converted.combinations(2).flatMap {
        case first :: second :: Nil => Set(ConceptInclusion(Conjunction(first, second), Bottom))
        case _                      => ??? //impossible
      }.toSet
    case ClassAssertion(_, cls, NamedIndividual(iri))                                                                                          => convertExpression(cls).map(concept =>
      ConceptInclusion(Nominal(WIndividual(iri.toString)), concept)).toSet
    case ObjectPropertyAssertion(_, ObjectProperty(prop), NamedIndividual(subj), NamedIndividual(obj))                                         =>
      Set(ConceptInclusion(Nominal(WIndividual(subj.toString)), ExistentialRestriction(Role(prop.toString), Nominal(WIndividual(obj.toString)))))
    case ObjectPropertyAssertion(_, ObjectInverseOf(ObjectProperty(prop)), NamedIndividual(obj), NamedIndividual(subj))                        =>
      Set(ConceptInclusion(Nominal(WIndividual(subj.toString)), ExistentialRestriction(Role(prop.toString), Nominal(WIndividual(obj.toString)))))
    case EquivalentObjectProperties(_, propertyExpressions)                                                                                    =>
      //FIXME handle inverse property expression?
      val properties = propertyExpressions.collect { case p @ ObjectProperty(_) => p }.toList
      properties.combinations(2).flatMap {
        case ObjectProperty(first) :: ObjectProperty(second) :: Nil =>
          val role1 = Role(first.toString)
          val role2 = Role(second.toString)
          Set(
            RoleInclusion(role1, role2),
            RoleInclusion(role2, role1),
            Rule(body = List(RoleAtom(role1, WVariable("x"), WVariable("y"))), head = List(RoleAtom(role2, WVariable("x"), WVariable("y")))),
            Rule(body = List(RoleAtom(role2, WVariable("x"), WVariable("y"))), head = List(RoleAtom(role1, WVariable("x"), WVariable("y"))))
          )
        case _                                                      => ??? //impossible
      }.toSet
    case SubObjectPropertyOf(_, ObjectProperty(subproperty), ObjectProperty(superproperty))                                                    =>
      val sub = Role(subproperty.toString)
      val sup = Role(superproperty.toString)
      Set(
        RoleInclusion(sub, sup),
        Rule(List(RoleAtom(sub, WVariable("x1"), WVariable("x2"))), List(RoleAtom(sup, WVariable("x1"), WVariable("x2")))))
    case SubObjectPropertyChainOf(_, ObjectProperty(first) :: ObjectProperty(second) :: Nil, ObjectProperty(superproperty))                    =>
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
    case SubObjectPropertyChainOf(_, (first @ ObjectProperty(_)) :: (second @ ObjectProperty(_)) :: more, (superproperty @ ObjectProperty(_))) =>
      val compositionProperty: OWLObjectProperty = ObjectProperty(s"$CompositionRolePrefix${first.getIRI}${second.getIRI}")
      convertAxiom(SubObjectPropertyChainOf(List(first, second), compositionProperty)) ++
        convertAxiom(SubObjectPropertyChainOf(compositionProperty :: more, superproperty))
    case DisjointObjectProperties(_, properties)                                                                                               =>
      properties.toList.combinations(2).flatMap {
        case first :: second :: Nil =>
          val firstRoleAtom = first match {
            case ObjectProperty(iri)                  => RoleAtom(Role(iri.toString), WVariable("x"), WVariable("y"))
            case ObjectInverseOf(ObjectProperty(iri)) => RoleAtom(Role(iri.toString), WVariable("y"), WVariable("x"))
          }
          val secondRoleAtom = second match {
            case ObjectProperty(iri)                  => RoleAtom(Role(iri.toString), WVariable("x"), WVariable("y"))
            case ObjectInverseOf(ObjectProperty(iri)) => RoleAtom(Role(iri.toString), WVariable("y"), WVariable("x"))
          }
          Set(
            Rule(body = List(firstRoleAtom, secondRoleAtom), head = List(ConceptAtom(Bottom, WVariable("x")), ConceptAtom(Bottom, WVariable("y"))))
          )
        case _                      => ??? //impossible
      }.toSet
    case TransitiveObjectProperty(_, ObjectProperty(property))                                                                                 =>
      val role = Role(property.toString)
      Set(
        RoleComposition(role, role, role),
        Rule(body = List(RoleAtom(role, WVariable("x1"), WVariable("x2")), RoleAtom(role, WVariable("x2"), WVariable("x3"))), head = List(RoleAtom(role, WVariable("x1"), WVariable("x3")))))
    case ReflexiveObjectProperty(_, ObjectProperty(property))                                                                                  => Set(
      ConceptInclusion(Top, SelfRestriction(Role(property.toString)))
    )
    case FunctionalObjectProperty(_, ObjectProperty(property))                                                                                 =>
      val role = Role(property.toString)
      Set(
        Rule(body = List(RoleAtom(role, WVariable("x"), WVariable("y1")), RoleAtom(role, WVariable("x"), WVariable("y2"))), head = List(SameIndividualsAtom(WVariable("y1"), WVariable("y2"))))
      )
    case InverseFunctionalObjectProperty(_, ObjectProperty(property))                                                                          =>
      val role = Role(property.toString)
      Set(
        Rule(body = List(RoleAtom(role, WVariable("x1"), WVariable("y")), RoleAtom(role, WVariable("x2"), WVariable("y"))), head = List(SameIndividualsAtom(WVariable("x1"), WVariable("x2"))))
      )
    case IrreflexiveObjectProperty(_, ObjectProperty(property))                                                                                =>
      val role = Role(property.toString)
      Set(
        Rule(body = List(RoleAtom(role, WVariable("x"), WVariable("x"))), head = List(ConceptAtom(BuiltIn.Bottom, WVariable("x"))))
      )
    case SymmetricObjectProperty(_, ObjectProperty(property))                                                                                  =>
      val role = Role(property.toString)
      Set(
        Rule(body = List(RoleAtom(role, WVariable("x"), WVariable("y"))), head = List(RoleAtom(role, WVariable("y"), WVariable("x"))))
      )
    case AsymmetricObjectProperty(_, ObjectProperty(property))                                                                                 =>
      val role = Role(property.toString)
      Set(
        Rule(body = List(RoleAtom(role, WVariable("x"), WVariable("y")), RoleAtom(role, WVariable("y"), WVariable("x"))), head = List(ConceptAtom(BuiltIn.Bottom, WVariable("x")), ConceptAtom(BuiltIn.Bottom, WVariable("y"))))
      )
    case ObjectPropertyDomain(_, ObjectProperty(property), ce)                                                                                 =>
      convertExpression(ce).map(concept =>
        ConceptInclusion(ExistentialRestriction(Role(property.toString), Top), concept)).toSet
    case ObjectPropertyRange(_, ObjectProperty(property), ce)                                                                                  =>
      convertExpression(ce).to(Set).flatMap { concept =>
        val role = Role(property.toString)
        Set(
          RoleHasRange(role, concept),
          Rule(body = List(RoleAtom(role, WVariable("x1"), WVariable("x2"))), head = List(ConceptAtom(concept, WVariable("x2"))))
        )
      }
    case InverseObjectProperties(_, ObjectProperty(p), ObjectProperty(q))                                                                      =>
      val (roleP, roleQ) = (Role(p.toString), Role(q.toString))
      val (x1, x2) = (WVariable("x1"), WVariable("x2"))
      Set(
        Rule(body = List(RoleAtom(roleP, x1, x2)), head = List(RoleAtom(roleQ, x2, x1))),
        Rule(body = List(RoleAtom(roleQ, x1, x2)), head = List(RoleAtom(roleP, x2, x1))))
    case NegativeObjectPropertyAssertion(_, ObjectProperty(p), NamedIndividual(x), NamedIndividual(y))                                         =>
      val xInd = WIndividual(x.toString)
      val yInd = WIndividual(y.toString)
      Set(
        Rule(body = List(RoleAtom(Role(p.toString), xInd, yInd)), head = List(ConceptAtom(Bottom, xInd), ConceptAtom(Bottom, yInd)))
      )
    case SameIndividual(_, individuals)                                                                                                        =>
      individuals.toList.combinations(2).flatMap {
        case NamedIndividual(first) :: NamedIndividual(second) :: Nil => Set(
          ConceptInclusion(Nominal(WIndividual(first.toString)), Nominal(WIndividual(second.toString))),
          ConceptInclusion(Nominal(WIndividual(second.toString)), Nominal(WIndividual(first.toString)))
        )
        case _                                                        => ??? //impossible
      }.toSet
    case DifferentIndividuals(_, individuals)                                                                                                  =>
      individuals.toList.combinations(2).flatMap {
        case NamedIndividual(first) :: NamedIndividual(second) :: Nil => Set(
          ConceptInclusion(Conjunction(Nominal(WIndividual(first.toString)), Nominal(WIndividual(second.toString))), Bottom),
        )
        case _                                                        => ??? //impossible
      }.toSet
    case DLSafeRule(_, body, head)                                                                                                             => (for {
      bodyAtoms <- convertAtomSet(body)
      headAtoms <- convertAtomSet(head)
    } yield Rule(bodyAtoms, headAtoms)).toSet
    case _                                                                                                                                     =>
      //println(s"Not supported: $other")
      Set.empty
  }

  def convertExpression(expression: OWLClassExpression): Option[Concept] = {
    expression match {
      case OWLThing                                                   => Some(Top)
      case OWLNothing                                                 => Some(Bottom)
      case Class(iri)                                                 => Some(AtomicConcept(iri.toString))
      case ObjectSomeValuesFrom(ObjectProperty(prop), filler)         => convertExpression(filler).map(ExistentialRestriction(Role(prop.toString), _))
      case ObjectAllValuesFrom(ObjectProperty(prop), filler)          => convertExpression(filler).map(UniversalRestriction(Role(prop.toString), _))
      case ObjectMaxCardinality(0, ObjectProperty(prop), filler)      => convertExpression(filler).map(c => Complement(ExistentialRestriction(Role(prop.toString), c)))
      case ObjectMaxCardinality(1, ObjectProperty(prop), filler)      => convertExpression(filler).map(MaxCardinalityRestriction(Role(prop.toString), _, 1))
      case ObjectHasSelf(ObjectProperty(prop))                        => Some(SelfRestriction(Role(prop.toString)))
      case ObjectIntersectionOf(operands) if operands.nonEmpty        =>
        def convert(items: List[Concept]): Concept = items match {
          case first :: second :: Nil  => Conjunction(first, second)
          case first :: second :: rest => Conjunction(first, convert(second :: rest))
          case first :: Nil            => first
        }
        // sort to make sure we create only one conjunction for the same list of operands
        operands.toList.sortWith((a, b) => a.compareTo(b) < 0).map(convertExpression).sequence.map(convert)
      case ObjectUnionOf(operands)                                    =>
        operands.toList.map(convertExpression).sequence.map(_.toSet).map(Disjunction)
      case ObjectComplementOf(concept)                                => convertExpression(concept).map(Complement)
      case ObjectOneOf(individuals)                                   =>
        val operands = individuals.collect {
          case NamedIndividual(iri) => Nominal(WIndividual(iri.toString))
        }
        if (operands.isEmpty) None
        else if (operands.size == 1) operands.headOption
        else Some(Disjunction(operands.toSet[Concept]))
      case ObjectHasValue(ObjectProperty(prop), NamedIndividual(ind)) => Some(ExistentialRestriction(Role(prop.toString), Nominal(WIndividual(ind.toString))))
      case DataSomeValuesFrom(DataProperty(prop), range)              => Some(DataRestriction(DataRole(prop.toString), DataRange(range)))
      //scowl is missing DataHasValue
      case dhv: OWLDataHasValue => Some(DataHasValue(DataRole(dhv.getProperty.asOWLDataProperty.getIRI.toString), dhv.getFiller))
      case _                    =>
        //println(s"Not supported: $other")
        None
    }
  }

  def convertAtomSet(atoms: Set[SWRLAtom]): Option[List[RuleAtom]] = atoms.toList.map(convertRuleAtom).sequence

  @tailrec
  def convertRuleAtom(atom: SWRLAtom): Option[RuleAtom] = atom match {
    case ClassAtom(ce, arg)                                                       => for {
      concept <- convertExpression(ce)
      argument <- convertAtomArg(arg)
    } yield ConceptAtom(concept, argument)
    case ObjectPropertyAtom(ObjectProperty(iri), subj, obj)                       => for {
      subject <- convertAtomArg(subj)
      target <- convertAtomArg(obj)
    } yield RoleAtom(Role(iri.toString), subject, target)
    case ObjectPropertyAtom(ObjectInverseOf(prop @ ObjectProperty(_)), subj, obj) =>
      convertRuleAtom(ObjectPropertyAtom(prop, obj, subj))
    case swrl.SameIndividualAtom(left, right)                                     => for {
      subject <- convertAtomArg(left)
      target <- convertAtomArg(right)
    } yield RoleAtom(SameAs, subject, target)
    case swrl.DifferentIndividualsAtom(left, right)                               => for {
      subject <- convertAtomArg(left)
      target <- convertAtomArg(right)
    } yield RoleAtom(DifferentFrom, subject, target)
    case _                                                                        => None
  }

  def convertAtomArg(arg: SWRLIArgument): Option[IndividualArgument] = arg match {
    case IndividualArg(NamedIndividual(iri)) => Some(WIndividual(iri.toString))
    case Variable(iri)                       => Some(WVariable(iri.toString))
    case _                                   => None
  }

  implicit class ListExtensions[T](val self: List[Option[T]]) extends AnyVal {

    def sequence: Option[List[T]] = if (self.contains(None)) None else Some(self.flatten)

  }

}
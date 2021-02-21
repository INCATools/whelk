package org.geneontology.whelk.archimedes

import org.geneontology.archimedes.owl.OWLVocabulary._
import org.geneontology.archimedes.owl.{Atom => SWRLAtom, Axiom => OWLAxiom, DataHasValue => OWLDataHasValue, NamedIndividual => OWLNamedIndividual, Variable => OWLVariable, _}
import org.geneontology.archimedes.util.Lists.PluralList
import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk._

import scala.annotation.tailrec

object Bridge {

  def ontologyToAxioms(ont: Ontology): Set[Axiom] = ont.axioms.flatMap(convertAxiom)

  def convertAxiom(owlAxiom: OWLAxiom): Set[Axiom] = owlAxiom match {
    case SubClassOf(subclass, superclass, _)                                                                                                        => (convertExpression(subclass), convertExpression(superclass)) match {
      case (Some(subConcept), Some(superConcept)) => Set(ConceptInclusion(subConcept, superConcept))
      case _                                      => Set.empty
    }
    case EquivalentClasses(operands, _)                                                                                                             =>
      val converted = operands.items.map(convertExpression).toList.collect { case Some(concept) => concept }
      converted.combinations(2).flatMap {
        case first :: second :: Nil => Set(ConceptInclusion(first, second), ConceptInclusion(second, first))
        case _                      => ??? //impossible
      }.toSet
    case DisjointClasses(operands, _) if operands.items.size == 2                                                                                   => //FIXME handle >2
      val converted = operands.items.map(convertExpression).toList.collect { case Some(concept) => concept }
      converted.combinations(2).flatMap {
        case first :: second :: Nil => Set(ConceptInclusion(Conjunction(first, second), Bottom))
        case _                      => ??? //impossible
      }.toSet
    case ClassAssertion(cls, OWLNamedIndividual(iri), _)                                                                                            => convertExpression(cls).map(concept =>
      ConceptInclusion(Nominal(Individual(iri.id)), concept)).toSet
    case ObjectPropertyAssertion(ObjectProperty(prop), OWLNamedIndividual(subj), OWLNamedIndividual(obj), _)                                        =>
      Set(ConceptInclusion(Nominal(Individual(subj.id)), ExistentialRestriction(Role(prop.id), Nominal(Individual(obj.id)))))
    case ObjectPropertyAssertion(ObjectInverseOf(ObjectProperty(prop)), OWLNamedIndividual(obj), OWLNamedIndividual(subj), _)                       =>
      Set(ConceptInclusion(Nominal(Individual(subj.id)), ExistentialRestriction(Role(prop.id), Nominal(Individual(obj.id)))))
    case EquivalentObjectProperties(propertyExpressions, _)                                                                                         =>
      val properties = propertyExpressions.items.collect { case p @ ObjectProperty(_) => p }.toList
      properties.combinations(2).flatMap {
        case ObjectProperty(first) :: ObjectProperty(second) :: Nil => Set(RoleInclusion(Role(first.id), Role(second.id)))
        case _                                                      => ??? //impossible
      }.toSet
    case SubObjectPropertyOf(ObjectProperty(subproperty), ObjectProperty(superproperty), _)                                                         =>
      val sub = Role(subproperty.id)
      val sup = Role(superproperty.id)
      Set(
        RoleInclusion(sub, sup),
        Rule(List(RoleAtom(sub, Variable("x1"), Variable("x2"))), List(RoleAtom(sup, Variable("x1"), Variable("x2")))))
    case SubObjectPropertyOf(ObjectPropertyChain(PluralList(ObjectProperty(first), ObjectProperty(second), Nil)), ObjectProperty(superproperty), _) => //FIXME handle >2
      val supRole = Role(superproperty.id)

      def makeSubject(level: Int): Variable = if (level == 0) Variable("x") else Variable(s"x$level")

      val subprops = List(first, second) //for future handling of size >2
      val start = makeSubject(0)
      val end = makeSubject(subprops.size)
      val atoms = for {
        (subprop, index) <- subprops.zipWithIndex
        subrole = Role(subprop.id)
      } yield RoleAtom(subrole, makeSubject(index), makeSubject(index + 1))
      Set(
        RoleComposition(Role(first.id), Role(second.id), supRole),
        Rule(body = atoms, head = List(RoleAtom(supRole, start, end))))
    case TransitiveObjectProperty(ObjectProperty(property), _)                                                                                      =>
      val role = Role(property.id)
      Set(
        RoleComposition(role, role, role),
        Rule(body = List(RoleAtom(role, Variable("x1"), Variable("x2")), RoleAtom(role, Variable("x2"), Variable("x3"))), head = List(RoleAtom(role, Variable("x1"), Variable("x3")))))
    case ReflexiveObjectProperty(ObjectProperty(property), _)                                                                                       => Set(
      ConceptInclusion(Top, SelfRestriction(Role(property.id)))
    )
    case ObjectPropertyDomain(ObjectProperty(property), ce, _)                                                                                      => convertExpression(ce).map(concept =>
      ConceptInclusion(ExistentialRestriction(Role(property.id), Top), concept)).toSet
    case ObjectPropertyRange(ObjectProperty(property), ce, _)                                                                                       => convertExpression(ce).map(concept =>
      //TODO only supporting in rules for now
      Rule(body = List(RoleAtom(Role(property.id), Variable("x1"), Variable("x2"))), head = List(ConceptAtom(concept, Variable("x2"))))).toSet
    case InverseObjectProperties(ObjectProperty(p), ObjectProperty(q), _)                                                                           =>
      val (roleP, roleQ) = (Role(p.id), Role(q.id))
      val (x1, x2) = (Variable("x1"), Variable("x2"))
      Set(
        Rule(body = List(RoleAtom(roleP, x1, x2)), head = List(RoleAtom(roleQ, x2, x1))),
        Rule(body = List(RoleAtom(roleQ, x1, x2)), head = List(RoleAtom(roleP, x2, x1))))
    case DLSafeRule(body, head, _)                                                                                                                  => (for {
      bodyAtoms <- convertAtomSet(body)
      headAtoms <- convertAtomSet(head)
    } yield Rule(bodyAtoms, headAtoms)).toSet
    case _                                                                                                                                          =>
      //println(s"Not supported: $other")
      Set.empty
  }

  //TODO ObjectOneOf
  def convertExpression(expression: ClassExpression): Option[Concept] = {
    expression match {
      case OWLThing                                                      => Some(Top)
      case OWLNothing                                                    => Some(Bottom)
      case Class(iri)                                                    => Some(AtomicConcept(iri.id))
      case ObjectSomeValuesFrom(ObjectProperty(prop), filler)            => convertExpression(filler).map(ExistentialRestriction(Role(prop.id), _))
      case ObjectHasSelf(ObjectProperty(prop))                           => Some(SelfRestriction(Role(prop.id)))
      case ObjectIntersectionOf(operands)                                =>
        def convert(items: List[Concept]): Concept = items match {
          case first :: second :: Nil  => Conjunction(first, second)
          case first :: second :: rest => Conjunction(first, convert(second :: rest))
          case first :: Nil            => first
        }
        // sort to make sure we create only one conjunction for the same list of operands
        operands.items.toList.sortBy(_.toString).map(convertExpression).sequence.map(convert)
      case ObjectUnionOf(operands)                                       =>
        operands.items.toList.map(convertExpression).sequence.map(_.toSet).map(Disjunction)
      case ObjectComplementOf(concept)                                   => convertExpression(concept).map(Complement)
      case ObjectOneOf(individuals) if individuals.items.size == 1       => individuals.items.collectFirst { case OWLNamedIndividual(iri) => Nominal(Individual(iri.id)) }
      case ObjectHasValue(ObjectProperty(prop), OWLNamedIndividual(ind)) => Some(ExistentialRestriction(Role(prop.id), Nominal(Individual(ind.id))))
      case DataSomeValuesFrom(DataProperty(prop), range)                 => Some(DataRestriction(DataRole(prop.id), DataRange(range)))
      //scowl is missing DataHasValue
      case OWLDataHasValue(prop, literal) => Some(DataHasValue(DataRole(prop.iri.id), literal))
      case _                              =>
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
    } yield RoleAtom(Role(iri.id), subject, target)
    case ObjectPropertyAtom(ObjectInverseOf(prop @ ObjectProperty(_)), subj, obj) => convertRuleAtom(ObjectPropertyAtom(prop, obj, subj))
    case _                                                                        => None
  }

  def convertAtomArg(arg: IArg): Option[IndividualArgument] = arg match {
    case OWLNamedIndividual(iri) => Some(Individual(iri.id))
    case AnonymousIndividual(id) => Some(Individual(id)) //FIXME should Whelk have AnonymousIndividual?
    case OWLVariable(iri)        => Some(Variable(iri.id))
  }

  implicit class ListExtensions[T](val self: List[Option[T]]) extends AnyVal {

    def sequence: Option[List[T]] = if (self.contains(None)) None else Some(self.flatten)

  }

}

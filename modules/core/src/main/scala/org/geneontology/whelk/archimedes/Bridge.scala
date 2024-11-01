package org.geneontology.whelk.archimedes

import org.geneontology.archimedes.owl.OWLVocabulary._
import org.geneontology.archimedes.owl.{Atom => SWRLAtom, Axiom => OWLAxiom, DataHasValue => OWLDataHasValue, NamedIndividual => OWLNamedIndividual, Variable => OWLVariable, DifferentIndividualsAtom => SWRLDifferentIndividualsAtom, _}
import org.geneontology.archimedes.util.Lists.PluralList
import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.Role.CompositionRolePrefix
import org.geneontology.whelk._

import scala.annotation.tailrec

object Bridge {

  def ontologyToAxioms(ont: Ontology): Set[Axiom] = ont.axioms.flatMap(convertAxiom)

  def convertAxiom(owlAxiom: OWLAxiom): Set[Axiom] = owlAxiom match {
    case SubClassOf(subclass, superclass, _)                                                                                                                                                    => (convertExpression(subclass), convertExpression(superclass)) match {
      case (Some(subConcept), Some(superConcept)) => Set(ConceptInclusion(subConcept, superConcept))
      case _                                      => Set.empty
    }
    case EquivalentClasses(operands, _)                                                                                                                                                         =>
      val converted = operands.items.map(convertExpression).toList.collect { case Some(concept) => concept }
      converted.combinations(2).flatMap {
        case first :: second :: Nil => Set(ConceptInclusion(first, second), ConceptInclusion(second, first))
        case _                      => Set.empty //impossible
      }.toSet
    case DisjointClasses(operands, _)                                                                                                                                                           =>
      val converted = operands.items.map(convertExpression).toList.collect { case Some(concept) => concept }
      converted.combinations(2).flatMap {
        case first :: second :: Nil => Set(ConceptInclusion(Conjunction(first, second), Bottom))
        case _                      => Set.empty //impossible
      }.toSet
    case ClassAssertion(cls, OWLNamedIndividual(iri), _)                                                                                                                                        => convertExpression(cls).map(concept =>
      ConceptInclusion(Nominal(Individual(iri.id)), concept)).toSet
    case ObjectPropertyAssertion(ObjectProperty(prop), OWLNamedIndividual(subj), OWLNamedIndividual(obj), _)                                                                                    =>
      Set(ConceptInclusion(Nominal(Individual(subj.id)), ExistentialRestriction(Role(prop.id), Nominal(Individual(obj.id)))))
    case ObjectPropertyAssertion(ObjectInverseOf(ObjectProperty(prop)), OWLNamedIndividual(obj), OWLNamedIndividual(subj), _)                                                                   =>
      Set(ConceptInclusion(Nominal(Individual(subj.id)), ExistentialRestriction(Role(prop.id), Nominal(Individual(obj.id)))))
    case EquivalentObjectProperties(propertyExpressions, _)                                                                                                                                     =>
      val properties = propertyExpressions.items.collect { case p @ ObjectProperty(_) => p }.toList
      properties.combinations(2).flatMap {
        case ObjectProperty(first) :: ObjectProperty(second) :: Nil =>
          val role1 = Role(first.id)
          val role2 = Role(second.id)
          Set(
            RoleInclusion(role1, role2),
            RoleInclusion(role2, role1),
            Rule(body = List(RoleAtom(role1, Variable("x"), Variable("y"))), head = List(RoleAtom(role2, Variable("x"), Variable("y")))),
            Rule(body = List(RoleAtom(role2, Variable("x"), Variable("y"))), head = List(RoleAtom(role1, Variable("x"), Variable("y"))))
          )
        case _                                                      => Set.empty //impossible
      }.toSet
    case SubObjectPropertyOf(ObjectProperty(subproperty), ObjectProperty(superproperty), _)                                                                                                     =>
      val sub = Role(subproperty.id)
      val sup = Role(superproperty.id)
      Set(
        RoleInclusion(sub, sup),
        Rule(List(RoleAtom(sub, Variable("x1"), Variable("x2"))), List(RoleAtom(sup, Variable("x1"), Variable("x2")))))
    case SubObjectPropertyOf(ObjectPropertyChain(PluralList(ObjectProperty(first), ObjectProperty(second), Nil)), ObjectProperty(superproperty), _)                                             =>
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
    case SubObjectPropertyOf(ObjectPropertyChain(PluralList(first @ ObjectProperty(_), second @ ObjectProperty(_), (third @ ObjectProperty(_)) :: rest)), superproperty @ ObjectProperty(_), _) =>
      val compositionProperty = ObjectProperty(IRI(s"$CompositionRolePrefix${first.iri.id}${second.iri.id}"))
      convertAxiom(SubObjectPropertyOf(ObjectPropertyChain(PluralList(first, second, Nil)), compositionProperty)) ++
        convertAxiom(SubObjectPropertyOf(ObjectPropertyChain(PluralList(compositionProperty, third, rest)), superproperty))
    case DisjointObjectProperties(properties, _)                                                                                                                                                =>
      properties.items.toList.combinations(2).flatMap {
        case first :: second :: Nil =>
          val firstRoleAtom = first match {
            case ObjectProperty(iri)                  => RoleAtom(Role(iri.id), Variable("x"), Variable("y"))
            case ObjectInverseOf(ObjectProperty(iri)) => RoleAtom(Role(iri.id), Variable("y"), Variable("x"))
          }
          val secondRoleAtom = second match {
            case ObjectProperty(iri)                  => RoleAtom(Role(iri.id), Variable("x"), Variable("y"))
            case ObjectInverseOf(ObjectProperty(iri)) => RoleAtom(Role(iri.id), Variable("y"), Variable("x"))
          }
          Set(
            Rule(body = List(firstRoleAtom, secondRoleAtom), head = List(ConceptAtom(Bottom, Variable("x")), ConceptAtom(Bottom, Variable("y"))))
          )
        case _                      => ??? //impossible
      }.toSet
    case TransitiveObjectProperty(ObjectProperty(property), _)                                                                                                                                  =>
      val role = Role(property.id)
      Set(
        RoleComposition(role, role, role),
        Rule(body = List(RoleAtom(role, Variable("x1"), Variable("x2")), RoleAtom(role, Variable("x2"), Variable("x3"))), head = List(RoleAtom(role, Variable("x1"), Variable("x3")))))
    case ReflexiveObjectProperty(ObjectProperty(property), _)                                                                                                                                   => Set(
      ConceptInclusion(Top, SelfRestriction(Role(property.id)))
    )
    case FunctionalObjectProperty(ObjectProperty(property), _)                                                                                                                                  =>
      val role = Role(property.id)
      Set(
        Rule(body = List(RoleAtom(role, Variable("x"), Variable("y1")), RoleAtom(role, Variable("x"), Variable("y2"))), head = List(SameIndividualsAtom(Variable("y1"), Variable("y2"))))
      )
    case InverseFunctionalObjectProperty(ObjectProperty(property), _)                                                                                                                           =>
      val role = Role(property.id)
      Set(
        Rule(body = List(RoleAtom(role, Variable("x1"), Variable("y")), RoleAtom(role, Variable("x2"), Variable("y"))), head = List(SameIndividualsAtom(Variable("x1"), Variable("x2"))))
      )
    case IrreflexiveObjectProperty(ObjectProperty(property), _)                                                                                                                                 =>
      val role = Role(property.id)
      Set(
        Rule(body = List(RoleAtom(role, Variable("x"), Variable("x"))), head = List(ConceptAtom(BuiltIn.Bottom, Variable("x"))))
      )
    case SymmetricObjectProperty(ObjectProperty(property), _)                                                                                                                                   =>
      val role = Role(property.id)
      Set(
        Rule(body = List(RoleAtom(role, Variable("x"), Variable("y"))), head = List(RoleAtom(role, Variable("y"), Variable("x"))))
      )
    case AsymmetricObjectProperty(ObjectProperty(property), _)                                                                                                                                  =>
      val role = Role(property.id)
      Set(
        Rule(body = List(RoleAtom(role, Variable("x"), Variable("y")), RoleAtom(role, Variable("y"), Variable("x"))), head = List(ConceptAtom(BuiltIn.Bottom, Variable("x")), ConceptAtom(BuiltIn.Bottom, Variable("y"))))
      )
    case ObjectPropertyDomain(ObjectProperty(property), ce, _)                                                                                                                                  =>
      convertExpression(ce).map(concept =>
        ConceptInclusion(ExistentialRestriction(Role(property.id), Top), concept)).toSet
    case ObjectPropertyRange(ObjectProperty(property), ce, _)                                                                                                                                   =>
      convertExpression(ce).to(Set).flatMap { concept =>
        val role = Role(property.id)
        Set(
          RoleHasRange(role, concept),
          Rule(body = List(RoleAtom(role, Variable("x1"), Variable("x2"))), head = List(ConceptAtom(concept, Variable("x2"))))
        )
      }
    case InverseObjectProperties(ObjectProperty(p), ObjectProperty(q), _)                                                                                                                       =>
      val (roleP, roleQ) = (Role(p.id), Role(q.id))
      val (x1, x2) = (Variable("x1"), Variable("x2"))
      Set(
        Rule(body = List(RoleAtom(roleP, x1, x2)), head = List(RoleAtom(roleQ, x2, x1))),
        Rule(body = List(RoleAtom(roleQ, x1, x2)), head = List(RoleAtom(roleP, x2, x1))))
    case NegativeObjectPropertyAssertion(ObjectProperty(p), OWLNamedIndividual(x), OWLNamedIndividual(y), _)                                                                                    =>
      val xInd = Individual(x.id)
      val yInd = Individual(y.id)
      Set(
        Rule(body = List(RoleAtom(Role(p.id), xInd, yInd)), head = List(ConceptAtom(Bottom, xInd), ConceptAtom(Bottom, yInd)))
      )
    case SameIndividual(individuals, _)                                                                                                                                                         =>
      individuals.items.toList.combinations(2).flatMap {
        case OWLNamedIndividual(first) :: OWLNamedIndividual(second) :: Nil => Set(
          ConceptInclusion(Nominal(Individual(first.id)), Nominal(Individual(second.id))),
          ConceptInclusion(Nominal(Individual(second.id)), Nominal(Individual(first.id)))
        )
        case _                                                              => ??? //impossible
      }.toSet
    case DifferentIndividuals(individuals, _)                                                                                                                                                   =>
      individuals.items.toList.combinations(2).flatMap {
        case OWLNamedIndividual(first) :: OWLNamedIndividual(second) :: Nil => Set(
          ConceptInclusion(Conjunction(Nominal(Individual(first.id)), Nominal(Individual(second.id))), Bottom),
        )
        case _                                                              => ??? //impossible
      }.toSet
    case DLSafeRule(body, head, _)                                                                                                                                                              => (for {
      bodyAtoms <- convertAtomSet(body)
      headAtoms <- convertAtomSet(head)
    } yield Rule(bodyAtoms, headAtoms)).toSet
    case _                                                                                                                                                                                      =>
      //println(s"Not supported: $other")
      Set.empty
  }

  def convertExpression(expression: ClassExpression): Option[Concept] = {
    expression match {
      case OWLThing                                                      => Some(Top)
      case OWLNothing                                                    => Some(Bottom)
      case Class(iri)                                                    => Some(AtomicConcept(iri.id))
      case ObjectSomeValuesFrom(ObjectProperty(prop), filler)            => convertExpression(filler).map(ExistentialRestriction(Role(prop.id), _))
      case ObjectAllValuesFrom(ObjectProperty(prop), filler)             => convertExpression(filler).map(UniversalRestriction(Role(prop.id), _))
      case ObjectMaxCardinality(0, ObjectProperty(prop), filler)         => convertExpression(filler).map(c => Complement(ExistentialRestriction(Role(prop.id), c)))
      case ObjectMaxCardinality(1, ObjectProperty(prop), filler)         => convertExpression(filler).map(MaxCardinalityRestriction(Role(prop.id), _, 1))
      case ObjectHasSelf(ObjectProperty(prop))                           => Some(SelfRestriction(Role(prop.id)))
      case ObjectIntersectionOf(operands)                                =>
        def convert(items: List[Concept]): Option[Concept] = items match {
          case first :: second :: Nil  => Some(Conjunction(first, second))
          case first :: second :: rest => convert(second :: rest).map(right => Conjunction(first, right))
          case first :: Nil            => Some(first)
          case Nil                     => None
        }
        // sort to make sure we create only one conjunction for the same list of operands
        operands.items.toList.sortBy(_.toString).map(convertExpression).sequence.flatMap(convert)
      case ObjectUnionOf(operands)                                       =>
        operands.items.toList.map(convertExpression).sequence.map(_.toSet).map(Disjunction)
      case ObjectComplementOf(concept)                                   => convertExpression(concept).map(Complement)
      case ObjectOneOf(individuals)                                      =>
        val operands = individuals.items.collect {
          case OWLNamedIndividual(iri) => Nominal(Individual(iri.id))
        }
        if (operands.isEmpty) None
        else if (operands.size == 1) operands.headOption
        else Some(Disjunction(operands.toSet[Concept]))
      case ObjectHasValue(ObjectProperty(prop), OWLNamedIndividual(ind)) => Some(ExistentialRestriction(Role(prop.id), Nominal(Individual(ind.id))))
      case DataSomeValuesFrom(DataProperty(prop), range)                 => Some(DataRestriction(DataRole(prop.id), DataRange(range)))
      case OWLDataHasValue(prop, literal)                                => Some(DataHasValue(DataRole(prop.iri.id), literal))
      case _                                                             => None
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
    case ObjectPropertyAtom(ObjectInverseOf(prop @ ObjectProperty(_)), subj, obj) =>
      convertRuleAtom(ObjectPropertyAtom(prop, obj, subj))
    case SameIndividualAtom(left, right)                                          => for {
      subject <- convertAtomArg(left)
      target <- convertAtomArg(right)
    } yield RoleAtom(SameAs, subject, target)
    case SWRLDifferentIndividualsAtom(left, right)                                => for {
      subject <- convertAtomArg(left)
      target <- convertAtomArg(right)
    } yield RoleAtom(DifferentFrom, subject, target)
    case _                                                                        => None
  }

  def convertAtomArg(arg: IArg): Option[IndividualArgument] = arg match {
    case OWLNamedIndividual(iri) => Some(Individual(iri.id))
    case AnonymousIndividual(_)  => None
    case OWLVariable(iri)        => Some(Variable(iri.id))
  }

  implicit class ListExtensions[T](val self: List[Option[T]]) extends AnyVal {

    def sequence: Option[List[T]] = if (self.contains(None)) None else Some(self.flatten)

  }

}

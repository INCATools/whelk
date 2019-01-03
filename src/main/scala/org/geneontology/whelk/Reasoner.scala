package org.geneontology.whelk

import BuiltIn._
import scala.annotation.tailrec

final case class ReasonerState(
                                hier: Map[Role, Set[Role]], // initial
                                hierComps: Map[Role, Map[Role, List[Role]]], // initial
                                assertions: List[ConceptInclusion],
                                todo: List[ConceptInclusion],
                                topOccursNegatively: Boolean,
                                inits: Set[Concept], // closure
                                assertedConceptInclusionsBySubclass: Map[Concept, List[ConceptInclusion]],
                                closureSubsBySuperclass: Map[Concept, Set[Concept]],
                                closureSubsBySubclass: Map[Concept, Set[Concept]],
                                assertedNegConjs: Set[Conjunction],
                                assertedNegConjsByOperandRight: Map[Concept, List[Conjunction]],
                                conjunctionsBySubclassesOfRightOperand: Map[Concept, Map[Concept, Set[Conjunction]]], // Map[subclassOfRightOperand, Map[leftOperand, Conjunction]]
                                linksBySubject: Map[Concept, Map[Role, Set[Concept]]],
                                linksByTarget: Map[Concept, Map[Role, List[Concept]]],
                                negExistsMapByConcept: Map[Concept, Set[ExistentialRestriction]],
                                propagations: Map[Concept, Map[Role, List[ExistentialRestriction]]],
                                ruleEngine: RuleEngine,
                                wm: WorkingMemory) {

  def subs: Set[ConceptInclusion] = closureSubsBySuperclass.flatMap {
    case (superclass, subclasses) =>
      subclasses.map(ConceptInclusion(_, superclass))
  }.toSet

  def classAssertions: Set[ConceptAssertion] = (for {
    (Nominal(ind), superclasses) <- closureSubsBySubclass
    a @ AtomicConcept(_) <- superclasses
  } yield ConceptAssertion(a, ind)).toSet

  def roleAssertions: Set[RoleAssertion] = (for {
    (Nominal(target), links) <- linksByTarget
    (role, subjects) <- links
    Nominal(subject) <- subjects
  } yield RoleAssertion(role, subject, target)).toSet

  def computeTaxonomy: Map[AtomicConcept, (Set[AtomicConcept], Set[AtomicConcept])] =
    closureSubsBySubclass.collect {
      case (c: AtomicConcept, subsumers) => c -> directSubsumers(c, subsumers)
    }

  def individualsDirectTypes: Map[Individual, Set[AtomicConcept]] =
    closureSubsBySubclass.collect {
      case (n @ Nominal(ind), subsumers) => ind -> directSubsumers(n, subsumers)._2
    }

  //FIXME clarify how these methods differ... names are confusing

  def directlySubsumes(concept: Concept): (Set[AtomicConcept], Set[AtomicConcept]) =
    direct(concept, closureSubsBySuperclass.getOrElse(concept, Set.empty) + Bottom, closureSubsBySuperclass.withDefaultValue(Set.empty[Concept]), Bottom)

  def directlySubsumedBy(concept: Concept): (Set[AtomicConcept], Set[AtomicConcept]) =
    direct(concept, closureSubsBySubclass.getOrElse(concept, Set.empty) + Top, closureSubsBySubclass.withDefaultValue(Set.empty[Concept]), Top)

  //  def directSubsumes(concept: Concept, allSubsConcepts: Set[Concept]): (Set[AtomicConcept], Set[AtomicConcept]) =
  //    direct(concept, allSubsConcepts + Bottom, closureSubsBySuperclass.withDefaultValue(Set.empty))

  def directSubsumers(concept: Concept, allSubsConcepts: Set[Concept]): (Set[AtomicConcept], Set[AtomicConcept]) =
    direct(concept, allSubsConcepts + Top, closureSubsBySubclass.withDefaultValue(Set.empty[Concept]), Top)

  def direct(concept: Concept, allSubsConcepts: Set[Concept], subsumerFunction: Concept => Concept => Boolean, tautology: Concept): (Set[AtomicConcept], Set[AtomicConcept]) = {
    val allSubsumers = allSubsConcepts.iterator.collect { case ac: AtomicConcept => ac }.filterNot(_ == concept)
    allSubsumers.foldLeft((Set.empty[AtomicConcept], Set.empty[AtomicConcept])) {
      case ((equivalents, directSubsumers), subsumer) =>
        if (concept == tautology || subsumerFunction(subsumer)(concept)) (equivalents + subsumer, directSubsumers)
        else {
          @tailrec
          def loop(currentDirectSubs: List[AtomicConcept], isDirect: Boolean, remove: List[AtomicConcept], add: List[AtomicConcept]): (List[AtomicConcept], List[AtomicConcept]) =
            (currentDirectSubs, isDirect, remove, add) match {
              case (Nil, true, removes, adds)                                                                        => (removes, subsumer :: adds)
              case (Nil, false, removes, adds)                                                                       => (removes, adds)
              case (other :: _, _, removes, adds) if subsumer == tautology || subsumerFunction(other)(subsumer)      => (removes, adds)
              case (other :: rest, direct, removes, adds) if other == tautology || subsumerFunction(subsumer)(other) => loop(rest, direct, other :: removes, adds)
              case (_ :: rest, direct, removes, adds)                                                                => loop(rest, direct, removes, adds)
            }

          val (remove, add) = loop(directSubsumers.toList, true, Nil, Nil)
          (equivalents, (directSubsumers -- remove) ++ add)
        }
    }
  }

}

object ReasonerState {

  val empty: ReasonerState = ReasonerState(Map.empty, Map.empty, Nil, Nil, false, Set.empty, Map.empty, Map(Bottom -> Set.empty), Map(Top -> Set.empty), Set.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, RuleEngine.empty, RuleEngine.empty.emptyMemory)

}

object Reasoner {

  def assert(axioms: Set[Axiom]): ReasonerState = {
    import scalaz.Scalaz._
    val allRoles = axioms.flatMap(_.signature).collect { case role: Role => role }
    val allRoleInclusions = axioms.collect { case ri: RoleInclusion => ri }
    val hier: Map[Role, Set[Role]] = saturateRoles(allRoleInclusions) |+| allRoles.map(r => r -> Set(r)).toMap
    val hierComps = indexRoleCompositions(hier, axioms.collect { case rc: RoleComposition => rc })
    val rules = axioms.collect { case r: Rule => r } //TODO create rules from certain Whelk axioms
    val anonymousRulePredicates = rules.flatMap(_.body.collect {
      case ConceptAtom(concept, _) if concept.isAnonymous => ConceptInclusion(concept, Top)
    })
    val concIncs = axioms.collect { case ci: ConceptInclusion => ci } ++ anonymousRulePredicates
    val ruleEngine = RuleEngine(rules)
    val wm = ruleEngine.emptyMemory
    assert(concIncs, ReasonerState.empty.copy(hier = hier, hierComps = hierComps, ruleEngine = ruleEngine, wm = wm))
  }

  def assert(axioms: Set[ConceptInclusion], reasoner: ReasonerState): ReasonerState = {
    val distinctConcepts = axioms.flatMap {
      case ConceptInclusion(subclass, superclass) => Set(subclass, superclass)
    }.flatMap(_.conceptSignature)
    val additionalAxioms = distinctConcepts.flatMap {
      case d @ Disjunction(_) => `R⊔`(d)
      case c @ Complement(_)  => `R¬`(c)
      case _                  => Set.empty[ConceptInclusion]
    }
    val updatedAssertions = additionalAxioms.toList ::: axioms.toList
    computeClosure(reasoner.copy(
      assertions = reasoner.assertions ::: updatedAssertions,
      todo = reasoner.todo ::: updatedAssertions))
  }

  @tailrec
  private[this] def computeClosure(reasoner: ReasonerState): ReasonerState = {
    if (reasoner.assertions.nonEmpty) {
      val item :: todoAssertions = reasoner.assertions
      computeClosure(processAssertedConceptInclusion(item, reasoner.copy(assertions = todoAssertions)))
    } else if (reasoner.todo.nonEmpty) {
      val ConceptInclusion(subclass, superclass) :: todo = reasoner.todo
      computeClosure(processConceptInclusion(subclass, superclass, reasoner.copy(todo = todo)))
    } else reasoner
  }

  private[this] def processAssertedConceptInclusion(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val ConceptInclusion(subclass, superclass) = ci
    val updated = reasoner.assertedConceptInclusionsBySubclass.updated(ci.subclass, ci :: reasoner.assertedConceptInclusionsBySubclass.getOrElse(ci.subclass, Nil))
    `R⊑left`(subclass, superclass, `R+∃a`(subclass, superclass, `R+⨅a`(subclass, superclass, `R⊤left`(subclass, superclass, reasoner.copy(assertedConceptInclusionsBySubclass = updated)))))
  }

  private[this] def processConcept(concept: Concept, reasoner: ReasonerState): ReasonerState =
    if (reasoner.inits(concept)) reasoner else
      `R⊤right`(concept, R0(concept, reasoner.copy(inits = reasoner.inits + concept)))

  private[whelk] def processConceptInclusion(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState = {
    val subs = reasoner.closureSubsBySuperclass.getOrElse(superclass, Set.empty)
    if (subs(subclass)) reasoner else {
      val closureSubsBySuperclass = reasoner.closureSubsBySuperclass.updated(superclass, subs + subclass)
      val supers = reasoner.closureSubsBySubclass.getOrElse(subclass, Set.empty)
      val closureSubsBySubclass = reasoner.closureSubsBySubclass.updated(subclass, supers + superclass)
      val updatedReasoner = `R⊑right`(subclass, superclass, `R+∃b-right`(subclass, superclass, `R-∃`(subclass, superclass, `R+⨅b-right`(subclass, superclass, `R+⨅right`(subclass, superclass, `R-⨅`(subclass, superclass, `R⊥left`(subclass, superclass, reasoner.copy(closureSubsBySuperclass = closureSubsBySuperclass, closureSubsBySubclass = closureSubsBySubclass))))))))
      subclass match {
        case Nominal(ind) => reasoner.ruleEngine.processConceptAssertion(ConceptAssertion(superclass, ind), updatedReasoner)
        case _            => updatedReasoner
      }
    }
  }

  private[this] def processSubPlus(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState = {
    val subs = reasoner.closureSubsBySuperclass.getOrElse(superclass, Set.empty)
    if (subs(subclass)) reasoner else {
      val closureSubsBySuperclass = reasoner.closureSubsBySuperclass.updated(superclass, subs + subclass)
      val supers = reasoner.closureSubsBySubclass.getOrElse(subclass, Set.empty)
      val closureSubsBySubclass = reasoner.closureSubsBySubclass.updated(subclass, supers + superclass)
      val updatedReasoner = `R⊑right`(subclass, superclass, `R+∃b-right`(subclass, superclass, `R+⨅b-right`(subclass, superclass, `R+⨅right`(subclass, superclass, `R⊥left`(subclass, superclass, reasoner.copy(closureSubsBySuperclass = closureSubsBySuperclass, closureSubsBySubclass = closureSubsBySubclass))))))
      subclass match {
        case Nominal(ind) => updatedReasoner.ruleEngine.processConceptAssertion(ConceptAssertion(superclass, ind), updatedReasoner)
        case _            => updatedReasoner
      }
    }
  }

  private[whelk] def processLink(subject: Concept, role: Role, target: Concept, reasoner: ReasonerState): ReasonerState = {
    val rolesToTargets = reasoner.linksBySubject.getOrElse(subject, Map.empty)
    val targetsSet = rolesToTargets.getOrElse(role, Set.empty[Concept])
    if (targetsSet(target)) reasoner else {
      val updatedTargetsSet = targetsSet + target
      val updatedRolesToTargets = rolesToTargets.updated(role, updatedTargetsSet)
      val linksBySubject = reasoner.linksBySubject.updated(subject, updatedRolesToTargets)
      val rolesToSubjects = reasoner.linksByTarget.getOrElse(target, Map.empty)
      val subjects = rolesToSubjects.getOrElse(role, Nil)
      val updatedSubjects = subject :: subjects
      val updatedRolesToSubjects = rolesToSubjects.updated(role, updatedSubjects)
      val linksByTarget = reasoner.linksByTarget.updated(target, updatedRolesToSubjects)
      val updatedReasoner = `R⤳`(subject, role, target, `R∘left`(subject, role, target, `R∘right`(subject, role, target, `R+∃right`(subject, role, target, `R⊥right`(subject, role, target, reasoner.copy(linksBySubject = linksBySubject, linksByTarget = linksByTarget))))))
      subject match {
        case Nominal(subjectInd) => target match {
          case Nominal(targetInd) => updatedReasoner.ruleEngine.processRoleAssertion(RoleAssertion(role, subjectInd, targetInd), updatedReasoner)
          case _                  => updatedReasoner
        }
        case _                   => updatedReasoner
      }
    }
  }

  private[this] def R0(concept: Concept, reasoner: ReasonerState): ReasonerState =
    processConceptInclusion(concept, concept, reasoner)

  private[this] def `R⊤left`(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState =
    if (subclass.signature(Top)) reasoner.copy(topOccursNegatively = true) else reasoner

  private[this] def `R⊤right`(concept: Concept, reasoner: ReasonerState): ReasonerState =
    if (reasoner.topOccursNegatively) processConceptInclusion(concept, Top, reasoner)
    else reasoner

  private[this] def `R⊑left`(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState =
    reasoner.closureSubsBySuperclass.getOrElse(subclass, Set.empty).foldLeft(reasoner) { (state, other) =>
      processConceptInclusion(other, superclass, state)
    }

  private[this] def `R⊑right`(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState =
    reasoner.assertedConceptInclusionsBySubclass.getOrElse(superclass, Nil).foldLeft(reasoner) { (state, other) =>
      processConceptInclusion(subclass, other.superclass, state)
    }

  private[this] def `R⊥left`(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState =
    if (superclass == Bottom) {
      reasoner.linksByTarget.getOrElse(subclass, Map.empty).foldLeft(reasoner) { case (state1, (_, subjects)) =>
        subjects.foldLeft(state1) { (state2, subject) =>
          processConceptInclusion(subject, Bottom, state2)
        }
      }
    } else reasoner

  private[this] def `R⊥right`(subject: Concept, role: Role, target: Concept, reasoner: ReasonerState): ReasonerState =
    if (reasoner.closureSubsBySuperclass(Bottom)(target))
      processConceptInclusion(subject, Bottom, reasoner)
    else reasoner

  private[this] def `R-⨅`(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState = superclass match {
    case Conjunction(left, right) =>
      processConceptInclusion(subclass, left,
        processConceptInclusion(subclass, right, reasoner))
    case _                        => reasoner
  }

  private[this] def `R+⨅a`(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState = {
    val newNegativeConjunctions = subclass.conceptSignature.collect { case conj: Conjunction => conj }.filterNot(reasoner.assertedNegConjs)
    val newAssertedNegConjs = reasoner.assertedNegConjs ++ newNegativeConjunctions
    val newNegConjsByOperandRight = newNegativeConjunctions.foldLeft(reasoner.assertedNegConjsByOperandRight) {
      case (acc, c @ Conjunction(_, right)) =>
        val updated = c :: acc.getOrElse(right, Nil)
        acc.updated(right, updated)
    }
    `R+⨅b-left`(newNegativeConjunctions, reasoner.copy(assertedNegConjs = newAssertedNegConjs, assertedNegConjsByOperandRight = newNegConjsByOperandRight))
  }

  private[this] def `R+⨅b-left`(newNegativeConjunctions: Iterable[Conjunction], reasoner: ReasonerState): ReasonerState = {
    var conjunctionsBySubclassesOfRightOperand = reasoner.conjunctionsBySubclassesOfRightOperand
    var newSubclassesAndConjunctions: List[(Concept, Conjunction)] = Nil
    for {
      conjunction <- newNegativeConjunctions
      cs = reasoner.closureSubsBySuperclass.getOrElse(conjunction.right, Set.empty)
      c <- cs
    } {
      newSubclassesAndConjunctions = (c -> conjunction) :: newSubclassesAndConjunctions
      val conjunctionsByLeft = conjunctionsBySubclassesOfRightOperand.getOrElse(c, Map.empty)
      val newConjunctionsForThisLeft = conjunctionsByLeft.getOrElse(conjunction.left, Set.empty) + conjunction
      val newValue = conjunctionsByLeft.updated(conjunction.left, newConjunctionsForThisLeft)
      conjunctionsBySubclassesOfRightOperand = conjunctionsBySubclassesOfRightOperand.updated(c, newValue)
    }
    `R+⨅left`(newSubclassesAndConjunctions, reasoner.copy(conjunctionsBySubclassesOfRightOperand = conjunctionsBySubclassesOfRightOperand))
  }

  private[this] def `R+⨅b-right`(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState = {
    var conjunctionsBySubclassesOfRightOperand = reasoner.conjunctionsBySubclassesOfRightOperand
    var newSubclassesAndConjunctions: List[(Concept, Conjunction)] = Nil
    val conjunctions = reasoner.assertedNegConjsByOperandRight.getOrElse(superclass, Nil)
    for {
      conjunction <- conjunctions
    } {
      newSubclassesAndConjunctions = (subclass -> conjunction) :: newSubclassesAndConjunctions
      val conjunctionsByLeft = conjunctionsBySubclassesOfRightOperand.getOrElse(subclass, Map.empty)
      val newConjunctionsForThisLeft = conjunctionsByLeft.getOrElse(conjunction.left, Set.empty) + conjunction
      val newValue = conjunctionsByLeft.updated(conjunction.left, newConjunctionsForThisLeft)
      conjunctionsBySubclassesOfRightOperand = conjunctionsBySubclassesOfRightOperand.updated(subclass, newValue)
    }
    `R+⨅left`(newSubclassesAndConjunctions, reasoner.copy(conjunctionsBySubclassesOfRightOperand = conjunctionsBySubclassesOfRightOperand))
  }

  private[this] def `R+⨅left`(subclassesAndConjunctions: Iterable[(Concept, Conjunction)], reasoner: ReasonerState): ReasonerState = {
    subclassesAndConjunctions.foldLeft(reasoner) { case (state, (c, conjunction)) =>
      val subclasses = reasoner.closureSubsBySuperclass.getOrElse(conjunction.left, Set.empty)
      if (subclasses(c)) processSubPlus(c, conjunction, state)
      else state
    }
  }

  private[this] def `R+⨅right`(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState = {
    val conjunctionsByLeft = reasoner.conjunctionsBySubclassesOfRightOperand.getOrElse(subclass, Map.empty)
    val conjunctions = conjunctionsByLeft.getOrElse(superclass, Set.empty)
    conjunctions.foldLeft(reasoner) { (state, conjunction) =>
      processSubPlus(subclass, conjunction, state)
    }
  }

  private[this] def `R-∃`(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState = superclass match {
    case ExistentialRestriction(role, filler) => processLink(subclass, role, filler, reasoner)
    case _                                    => reasoner
  }

  private[this] def `R+∃a`(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState = {
    val newNegativeExistentials = subclass.conceptSignature.collect { case er: ExistentialRestriction => er }
    val negExistsMapByConcept = newNegativeExistentials.foldLeft(reasoner.negExistsMapByConcept) { (acc, er) =>
      val updated = acc.getOrElse(er.concept, Set.empty) + er
      acc.updated(er.concept, updated)
    }
    `R+∃b-left`(newNegativeExistentials, reasoner.copy(negExistsMapByConcept = negExistsMapByConcept))
  }

  private[this] def `R+∃b-left`(newNegativeExistentials: Iterable[ExistentialRestriction], reasoner: ReasonerState): ReasonerState = {
    var newPropagations: List[(Concept, ExistentialRestriction)] = Nil
    var propagations = reasoner.propagations
    for {
      er <- newNegativeExistentials
      subclass <- reasoner.closureSubsBySuperclass.getOrElse(er.concept, Set.empty)
    } {
      newPropagations = (subclass, er) :: newPropagations
      val current = propagations.getOrElse(subclass, Map.empty)
      val newList = er :: current.getOrElse(er.role, Nil)
      propagations = propagations.updated(subclass, current.updated(er.role, newList))
    }
    `R+∃left`(newPropagations, reasoner.copy(propagations = propagations))
  }

  private[this] def `R+∃b-right`(subclass: Concept, superclass: Concept, reasoner: ReasonerState): ReasonerState = { //speed up
    var newPropagations: List[(Concept, ExistentialRestriction)] = Nil
    var propagations = reasoner.propagations
    val ers = reasoner.negExistsMapByConcept.getOrElse(superclass, Set.empty)
    for {
      er <- ers
    } {
      newPropagations = (subclass, er) :: newPropagations
      val current = propagations.getOrElse(subclass, Map.empty)
      val newList = er :: current.getOrElse(er.role, Nil)
      propagations = propagations.updated(subclass, current.updated(er.role, newList))
    }
    `R+∃left`(newPropagations, reasoner.copy(propagations = propagations))
  }

  private[this] def `R+∃left`(newPropagations: Iterable[(Concept, ExistentialRestriction)], reasoner: ReasonerState): ReasonerState = {
    newPropagations.foldLeft(reasoner) { case (state1, (concept, er)) =>
      reasoner.linksByTarget.getOrElse(concept, Map.empty).foldLeft(state1) { case (state2, (role, subjects)) =>
        if (reasoner.hier.getOrElse(role, Set.empty)(er.role))
          subjects.foldLeft(state2) { (state3, subject) =>
            processSubPlus(subject, er, state3)
          }
        else state2
      }
    }
  }

  private[this] def `R+∃right`(subject: Concept, role: Role, target: Concept, reasoner: ReasonerState): ReasonerState = {
    val roleToER = reasoner.propagations.getOrElse(target, Map.empty)
    reasoner.hier.getOrElse(role, Set.empty).foldLeft(reasoner) { (state1, s) =>
      roleToER.getOrElse(s, Nil).foldLeft(state1) { (state2, f) =>
        processSubPlus(subject, f, state2)
      }
    }
  }

  private[this] def `R∘left`(subject: Concept, role: Role, target: Concept, reasoner: ReasonerState): ReasonerState = {
    reasoner.linksByTarget.getOrElse(subject, Map.empty).foldLeft(reasoner) { case (state1, (r1, es)) =>
      val r1s = reasoner.hierComps.getOrElse(r1, Map.empty)
      r1s.getOrElse(role, Nil).foldLeft(state1) { (state2, s) =>
        es.foldLeft(state2) { (state3, e) =>
          processLink(e, s, target, state3)
        }
      }
    }
  }

  private[this] def `R∘right`(subject: Concept, role: Role, target: Concept, reasoner: ReasonerState): ReasonerState = {
    val r2s = reasoner.hierComps.getOrElse(role, Map.empty)
    reasoner.linksBySubject.getOrElse(target, Map.empty).foldLeft(reasoner) { case (state1, (r2, targets)) =>
      r2s.getOrElse(r2, Nil).foldLeft(state1) { (state2, s) =>
        targets.foldLeft(state2) { (state3, d) =>
          processLink(subject, s, d, state3)
        }
      }
    }
  }

  private[this] def `R⤳`(subject: Concept, role: Role, target: Concept, reasoner: ReasonerState): ReasonerState =
    processConcept(target, reasoner)

  private[this] def `R⊔`(d: Disjunction): Set[ConceptInclusion] = d.operands.map(o => ConceptInclusion(o, d))

  private[this] def `R¬`(c: Complement): Set[ConceptInclusion] = Set(ConceptInclusion(Conjunction(c.concept, c), Bottom))

  private[this] def saturateRoles(roleInclusions: Set[RoleInclusion]): Map[Role, Set[Role]] = { //FIXME can do this better?
    val subToSuper = roleInclusions.groupBy(_.subproperty).map { case (sub, ri) => sub -> ri.map(_.superproperty) }

    def allSupers(role: Role): Set[Role] = for {
      superProp <- subToSuper.getOrElse(role, Set.empty)
      superSuperProp <- allSupers(superProp) + superProp
    } yield superSuperProp

    subToSuper.keys.map(role => role -> allSupers(role)).toMap
  }

  private[this] def indexRoleCompositions(hier: Map[Role, Set[Role]], chains: Set[RoleComposition]): Map[Role, Map[Role, List[Role]]] = {
    val roleComps = chains.groupBy(rc => (rc.first, rc.second)).map {
      case (key, ris) =>
        key -> ris.map(_.superproperty)
    }
    val hierCompsTuples = (for {
      (r1, s1s) <- hier
      s1 <- s1s
      (r2, s2s) <- hier
      s2 <- s2s
      s <- roleComps.getOrElse((s1, s2), Set.empty)
    } yield (r1, r2, s)).toSet
    val hierCompsRemove = for {
      (r1, r2, s) <- hierCompsTuples
      superS <- hier(s)
      if superS != s
      if hierCompsTuples((r1, r2, superS))
    } yield (r1, r2, superS)
    val hierComps = (hierCompsTuples -- hierCompsRemove).groupBy(_._1).map {
      case (r1, values) => r1 -> values.map {
        case (_, r2, s) => (r2, s)
      }.groupBy(_._1).map {
        case (r2, ss) => r2 -> ss.map(_._2).toList
      }
    }
    hierComps
  }

}
package org.geneontology.whelk

import org.geneontology.whelk.BuiltIn._
import org.geneontology.whelk.Reasoner.QueueDelegate
import org.geneontology.whelk.Util.MapExtensions

import scala.annotation.tailrec

final case class ReasonerState(
                                hier: Map[Role, Set[Role]] = Map.empty, // initial
                                hierComps: Map[Role, Map[Role, List[Role]]] = Map.empty, // initial
                                assertions: List[ConceptInclusion] = Nil,
                                todo: List[QueueExpression] = Nil,
                                inits: Set[Concept] = Set.empty, // closure
                                assertedConceptInclusionsBySubclass: Map[Concept, List[ConceptInclusion]] = Map.empty,
                                closureSubsBySuperclass: Map[Concept, Set[Concept]] = Map(Bottom -> Set.empty),
                                closureSubsBySubclass: Map[Concept, Set[Concept]] = Map(Top -> Set.empty),
                                assertedNegConjs: Set[Conjunction] = Set.empty,
                                assertedNegConjsByOperandRight: Map[Concept, Map[Concept, Conjunction]] = Map.empty,
                                assertedNegConjsByOperandLeft: Map[Concept, Map[Concept, Conjunction]] = Map.empty,
                                assertedUnions: Set[Disjunction] = Set.empty,
                                unionsByOperand: Map[Concept, List[Disjunction]] = Map.empty,
                                linksBySubject: Map[Concept, Map[Role, Set[Concept]]] = Map.empty,
                                linksByTarget: Map[Concept, Map[Role, List[Concept]]] = Map.empty,
                                negExistsMapByConcept: Map[Concept, Set[ExistentialRestriction]] = Map.empty,
                                propagations: Map[Concept, Map[Role, List[ExistentialRestriction]]] = Map.empty,
                                assertedNegativeSelfRestrictionsByRole: Map[Role, SelfRestriction] = Map.empty,
                                ruleEngine: RuleEngine = RuleEngine.empty,
                                wm: WorkingMemory = RuleEngine.empty.emptyMemory,
                                disableBottom: Boolean = false,
                                queueDelegates: Map[String, QueueDelegate] = Map.empty) {

  def subs: Set[ConceptInclusion] = closureSubsBySuperclass.flatMap {
    case (superclass, subclasses) =>
      subclasses.map(ConceptInclusion(_, superclass))
  }.toSet ++
    inits.map(t => ConceptInclusion(t, Top)) ++
    inits.map(t => ConceptInclusion(Bottom, t)) +
    ConceptInclusion(Bottom, Top) + ConceptInclusion(Bottom, Bottom) + ConceptInclusion(Top, Top)

  def classAssertions: Set[ConceptAssertion] = (for {
    (Nominal(ind), superclasses) <- closureSubsBySubclass
    a @ AtomicConcept(_) <- superclasses
  } yield ConceptAssertion(a, ind)).toSet

  def directClassAssertions: Set[ConceptAssertion] = (for {
    (n @ Nominal(ind), superclasses) <- closureSubsBySubclass
    (equiv, types) = directSubsumers(n, superclasses)
    typ <- types
  } yield ConceptAssertion(typ, ind)).toSet

  def roleAssertions: Set[RoleAssertion] = (for {
    (Nominal(target), links) <- linksByTarget
    (role, subjects) <- links
    Nominal(subject) <- subjects
  } yield RoleAssertion(role, subject, target)).toSet

  def computeTaxonomy: Map[AtomicConcept, (Set[AtomicConcept], Set[AtomicConcept])] = {
    // Using the regular algorithm for Bottom takes too long
    val directSuperClassesOfBottom = closureSubsBySuperclass.collect { case (superclass: AtomicConcept, subclasses) if (subclasses - Bottom - superclass).forall(_.isAnonymous) => superclass }.toSet
    val equivalentsToBottom = closureSubsBySuperclass(Bottom).collect { case subclass: AtomicConcept => subclass }
    closureSubsBySubclass.collect {
      case (c: AtomicConcept, subsumers) => c -> directSubsumers(c, subsumers + Top)
    } + (Bottom -> (equivalentsToBottom, directSuperClassesOfBottom))
  }

  // redundant with directClassAssertions
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
    direct(concept, allSubsConcepts, closureSubsBySubclass.updated(Bottom, inits).withDefaultValue(Set.empty[Concept]), Top) //FIXME adding Bottom to this map should probably happen everywhere (or in reasoning)

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

  val empty: ReasonerState = ReasonerState()

}

object Reasoner {

  def assert(axioms: Set[Axiom], delegates: Map[String, QueueDelegate] = Map.empty, disableBottom: Boolean = false): ReasonerState = {
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
    assert(concIncs, ReasonerState.empty.copy(hier = hier, hierComps = hierComps, ruleEngine = ruleEngine, wm = wm, queueDelegates = delegates, disableBottom = disableBottom))
  }

  def assert(axioms: Set[ConceptInclusion], reasoner: ReasonerState): ReasonerState = {
    val distinctConcepts = axioms.flatMap {
      case ConceptInclusion(subclass, superclass) => Set(subclass, superclass)
    }.flatMap(_.conceptSignature)
    val atomicConcepts = distinctConcepts.collect { case a: AtomicConcept => a }
    val additionalAxioms = distinctConcepts.flatMap {
      case d @ Disjunction(_) => `R⊔`(d)
      case c @ Complement(_)  => `R¬`(c)
      case _                  => Set.empty[ConceptInclusion]
    }
    val negativeSelfRestrictions = axioms.flatMap(_.subclass.conceptSignature).collect { case sr: SelfRestriction => sr.role -> sr }.toMap
    val updatedAssertions = additionalAxioms.toList ::: axioms.toList
    computeClosure(reasoner.copy(
      assertions = reasoner.assertions ::: updatedAssertions,
      todo = reasoner.todo ::: atomicConcepts.toList ::: updatedAssertions,
      assertedNegativeSelfRestrictionsByRole = negativeSelfRestrictions))
  }

  @tailrec
  private[this] def computeClosure(reasoner: ReasonerState): ReasonerState = {
    if (reasoner.assertions.nonEmpty) {
      val item :: todoAssertions = reasoner.assertions
      computeClosure(processAssertedConceptInclusion(item, reasoner.copy(assertions = todoAssertions)))
    } else if (reasoner.todo.nonEmpty) {
      val item :: todo = reasoner.todo
      computeClosure(process(item, reasoner.copy(todo = todo)))
    } else reasoner
  }

  private[this] def processAssertedConceptInclusion(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val updated = reasoner.assertedConceptInclusionsBySubclass.updated(ci.subclass, ci :: reasoner.assertedConceptInclusionsBySubclass.getOrElse(ci.subclass, Nil))
    `R⊔aleft`(ci, `R⊑left`(ci, `R+∃a`(ci, `R+⨅a`(ci, reasoner.copy(assertedConceptInclusionsBySubclass = updated)))))
  }

  private[this] def process(expression: QueueExpression, reasoner: ReasonerState): ReasonerState = {
    expression match {
      case link: Link                   => processLink(link, reasoner)
      case ci: ConceptInclusion         => processConceptInclusion(ci, reasoner)
      case `Sub+`(ci: ConceptInclusion) => processSubPlus(ci, reasoner)
      case concept: Concept             => processConcept(concept, reasoner)
    }
  }

  private[this] def processConcept(concept: Concept, reasoner: ReasonerState): ReasonerState = {
    if (reasoner.inits(concept)) reasoner
    else {
      val newState = `R⊤`(concept, R0(concept, reasoner.copy(inits = reasoner.inits + concept)))
      newState.queueDelegates.keysIterator.foldLeft(newState) { (state, delegateKey) =>
        state.queueDelegates(delegateKey).processConcept(concept, state)
      }
    }
  }

  private[this] def processConceptInclusion(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val ConceptInclusion(subclass, superclass) = ci
    val subs = reasoner.closureSubsBySuperclass.getOrElse(superclass, Set.empty)
    if (subs(subclass)) reasoner else {
      val closureSubsBySuperclass = reasoner.closureSubsBySuperclass.updated(superclass, subs + subclass)
      val supers = reasoner.closureSubsBySubclass.getOrElse(subclass, Set.empty)
      val closureSubsBySubclass = reasoner.closureSubsBySubclass.updated(subclass, supers + superclass)
      val updatedReasoner = `R⊔right`(ci, `R+⟲`(ci, `R-⟲`(ci, `R⊑right`(ci, `R+∃b-right`(ci, `R-∃`(ci, `R+⨅left`(ci, `R+⨅right`(ci, `R-⨅`(ci, `R⊥left`(ci, reasoner.copy(closureSubsBySuperclass = closureSubsBySuperclass, closureSubsBySubclass = closureSubsBySubclass)))))))))))
      val newState = ci match {
        case ConceptInclusion(Nominal(ind), concept) => reasoner.ruleEngine.processConceptAssertion(ConceptAssertion(concept, ind), updatedReasoner)
        case _                                       => updatedReasoner
      }
      newState.queueDelegates.keysIterator.foldLeft(newState) { (state, delegateKey) =>
        state.queueDelegates(delegateKey).processConceptInclusion(ci, state)
      }
    }
  }

  private[this] def processSubPlus(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val ConceptInclusion(subclass, superclass) = ci
    val subs = reasoner.closureSubsBySuperclass.getOrElse(superclass, Set.empty)
    if (subs(subclass)) reasoner else {
      val closureSubsBySuperclass = reasoner.closureSubsBySuperclass.updated(superclass, subs + subclass)
      val supers = reasoner.closureSubsBySubclass.getOrElse(subclass, Set.empty)
      val closureSubsBySubclass = reasoner.closureSubsBySubclass.updated(subclass, supers + superclass)
      val updatedReasoner = `R⊔right`(ci, `R-⟲`(ci, `R⊑right`(ci, `R+∃b-right`(ci, `R+⨅left`(ci, `R+⨅right`(ci, `R⊥left`(ci, reasoner.copy(closureSubsBySuperclass = closureSubsBySuperclass, closureSubsBySubclass = closureSubsBySubclass))))))))
      val newState = ci match {
        case ConceptInclusion(Nominal(ind), concept) => updatedReasoner.ruleEngine.processConceptAssertion(ConceptAssertion(concept, ind), updatedReasoner)
        case _                                       => updatedReasoner
      }
      newState.queueDelegates.keysIterator.foldLeft(newState) { (state, delegateKey) =>
        state.queueDelegates(delegateKey).processSubPlus(ci, state)
      }
    }
  }

  private[this] def processLink(link: Link, reasoner: ReasonerState): ReasonerState = {
    val Link(subject, role, target) = link
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
      val updatedReasoner = `R+⟲𝒪`(link, `R⤳`(link, `R∘left`(link, `R∘right`(link, `R+∃right`(link, `R⊥right`(link, reasoner.copy(linksBySubject = linksBySubject, linksByTarget = linksByTarget)))))))
      val newState = link match {
        case Link(Nominal(subjectInd), aRole, Nominal(targetInd)) => updatedReasoner.ruleEngine.processRoleAssertion(RoleAssertion(aRole, subjectInd, targetInd), updatedReasoner)
        case _                                                    => updatedReasoner
      }
      newState.queueDelegates.keysIterator.foldLeft(newState) { (state, delegateKey) =>
        state.queueDelegates(delegateKey).processLink(link, state)
      }
    }
  }

  private[this] def R0(concept: Concept, reasoner: ReasonerState): ReasonerState =
    reasoner.copy(todo = ConceptInclusion(concept, concept) :: reasoner.todo)

  // In the ELK paper this rule is only applied if Top occurs negatively.
  // It simplifies constructing the node taxonomy to just apply it always.
  private[this] def `R⊤`(concept: Concept, reasoner: ReasonerState): ReasonerState =
    reasoner.copy(todo = ConceptInclusion(concept, Top) :: reasoner.todo)

  private[this] def `R⊑left`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    reasoner.closureSubsBySuperclass.getOrElse(ci.subclass, Set.empty).foreach { other =>
      todo = ConceptInclusion(other, ci.superclass) :: todo
    }
    reasoner.copy(todo = todo)
  }

  private[this] def `R⊔aleft`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    //TODO possibly restrict to superclass only
    val newUnions = (ci.subclass.conceptSignature ++ ci.superclass.conceptSignature).collect { case union: Disjunction => union }.filterNot(reasoner.assertedUnions)
    val updatedAssertedUnions = reasoner.assertedUnions ++ newUnions
    val updatedUnionsByOperand = newUnions.foldLeft(reasoner.unionsByOperand) {
      case (ubo1, d @ Disjunction(operands)) =>
        operands.foldLeft(ubo1) {
          case (ubo2, operand) =>
            val updated = d :: ubo2.getOrElse(operand, Nil)
            ubo2.updated(operand, updated)
        }
    }
    `R⊔bleft`(newUnions, reasoner.copy(assertedUnions = updatedAssertedUnions, unionsByOperand = updatedUnionsByOperand))
  }

  private[this] def `R⊔bleft`(newAssertedUnions: Iterable[Disjunction], reasoner: ReasonerState): ReasonerState = {
    var todoAssertions = reasoner.assertions
    var todo = reasoner.todo
    for {
      d @ Disjunction(operands) <- newAssertedUnions
      _ = operands.foreach(o => todo = o :: todo)
      superclassInCommon <- operands.map(o => reasoner.closureSubsBySubclass.getOrElse(o, Set.empty)).reduce(_ intersect _)
    } todoAssertions = ConceptInclusion(d, superclassInCommon) :: todoAssertions
    reasoner.copy(assertions = todoAssertions, todo = todo)
  }

  private[this] def `R⊔right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var todoAssertions = reasoner.assertions
    for {
      d @ Disjunction(operands) <- reasoner.unionsByOperand.getOrElse(ci.subclass, Nil)
      if operands.forall(o => o == ci.subclass || reasoner.closureSubsBySubclass.getOrElse(o, Set.empty)(ci.superclass))
    } todoAssertions = ConceptInclusion(d, ci.superclass) :: todoAssertions
    reasoner.copy(assertions = todoAssertions)
  }

  private[this] def `R⊑right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    reasoner.assertedConceptInclusionsBySubclass.getOrElse(ci.superclass, Nil).foreach { other =>
      todo = ConceptInclusion(ci.subclass, other.superclass) :: todo
    }
    reasoner.copy(todo = todo)
  }

  private[this] def `R⊥left`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState =
    if (reasoner.disableBottom) reasoner
    else if (ci.superclass == Bottom) {
      var todo = reasoner.todo
      for {
        (_, subjects) <- reasoner.linksByTarget.getOrElse(ci.subclass, Map.empty)
        subject <- subjects
      } todo = ConceptInclusion(subject, Bottom) :: todo
      reasoner.copy(todo = todo)
    } else reasoner

  private[this] def `R⊥right`(link: Link, reasoner: ReasonerState): ReasonerState =
    if (reasoner.disableBottom) reasoner
    else if (reasoner.closureSubsBySuperclass(Bottom)(link.target))
      reasoner.copy(todo = ConceptInclusion(link.subject, Bottom) :: reasoner.todo)
    else reasoner

  private[this] def `R-⨅`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = ci match {
    case ConceptInclusion(sub, Conjunction(left, right)) => reasoner.copy(todo = ConceptInclusion(sub, left) :: ConceptInclusion(sub, right) :: reasoner.todo)
    case _                                               => reasoner
  }

  private[this] def `R+⨅a`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val newNegativeConjunctions = ci.subclass.conceptSignature.collect { case conj: Conjunction => conj }.filterNot(reasoner.assertedNegConjs)
    val updatedAssertedNegConjs = reasoner.assertedNegConjs ++ newNegativeConjunctions
    val (updatedByLeft, updatedByRight) = newNegativeConjunctions.foldLeft(
      (reasoner.assertedNegConjsByOperandLeft, reasoner.assertedNegConjsByOperandRight)) {
      case ((accAssertedNegConjsByOperandLeftMap, accAssertedNegConjsByOperandRightMap), c) =>
        val byRightForLeft = accAssertedNegConjsByOperandLeftMap.getOrElse(c.left, Map.empty)
        val updatedAssertedNegConjsByOperandLeftMap = accAssertedNegConjsByOperandLeftMap.updated(c.left, byRightForLeft.updated(c.right, c))
        val byLeftForRight = accAssertedNegConjsByOperandRightMap.getOrElse(c.right, Map.empty)
        val updatedAssertedNegConjsByOperandRightMap = accAssertedNegConjsByOperandRightMap.updated(c.right, byLeftForRight.updated(c.left, c))
        (updatedAssertedNegConjsByOperandLeftMap, updatedAssertedNegConjsByOperandRightMap)
    }
    `R+⨅b`(newNegativeConjunctions, reasoner.copy(assertedNegConjs = updatedAssertedNegConjs,
      assertedNegConjsByOperandLeft = updatedByLeft,
      assertedNegConjsByOperandRight = updatedByRight))
  }

  private[this] def `R+⨅b`(newNegativeConjunctions: Iterable[Conjunction], reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      conjunction <- newNegativeConjunctions
      leftSubclasses = reasoner.closureSubsBySuperclass.getOrElse(conjunction.left, Set.empty)
      rightSubclasses = reasoner.closureSubsBySuperclass.getOrElse(conjunction.right, Set.empty)
      common = leftSubclasses.intersect(rightSubclasses)
      c <- common
    } todo = `Sub+`(ConceptInclusion(c, conjunction)) :: todo
    reasoner.copy(todo = todo)
  }

  private[this] def `R+⨅left`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val d1 = ci.superclass
    val c = ci.subclass
    val d2s = reasoner.closureSubsBySubclass(c)
    val conjunctionsMatchingLeft = reasoner.assertedNegConjsByOperandLeft.getOrElse(d1, Map.empty)
    var todo = reasoner.todo
    // choose a join order: can make a massive performance difference
    if (d2s.size < conjunctionsMatchingLeft.size) {
      // better for PRO
      for {
        d2 <- d2s
        conjunction <- conjunctionsMatchingLeft.get(d2)
      } todo = `Sub+`(ConceptInclusion(c, conjunction)) :: todo
    } else {
      // better for GO
      for {
        (right, conjunction) <- conjunctionsMatchingLeft
        if (d2s(right))
      } todo = `Sub+`(ConceptInclusion(c, conjunction)) :: todo
    }
    reasoner.copy(todo = todo)
  }

  private[this] def `R+⨅right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val d2 = ci.superclass
    val c = ci.subclass
    val d1s = reasoner.closureSubsBySubclass(c)
    val conjunctionsMatchingRight = reasoner.assertedNegConjsByOperandRight.getOrElse(d2, Map.empty)
    var todo = reasoner.todo
    // choose a join order: can make a massive performance difference
    if (d1s.size < conjunctionsMatchingRight.size) {
      for {
        d1 <- d1s
        conjunction <- conjunctionsMatchingRight.get(d1)
      } todo = `Sub+`(ConceptInclusion(c, conjunction)) :: todo
    } else {
      for {
        (left, conjunction) <- conjunctionsMatchingRight
        if (d1s(left))
      } todo = `Sub+`(ConceptInclusion(c, conjunction)) :: todo
    }
    reasoner.copy(todo = todo)
  }

  private[this] def `R-∃`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = ci match {
    case ConceptInclusion(c, ExistentialRestriction(role, filler)) => reasoner.copy(todo = Link(c, role, filler) :: reasoner.todo)
    case _                                                         => reasoner
  }

  private[this] def `R+∃a`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val newNegativeExistentials = ci.subclass.conceptSignature.collect { case er: ExistentialRestriction => er }
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

  private[this] def `R+∃b-right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = { //speed up
    var newPropagations: List[(Concept, ExistentialRestriction)] = Nil
    var propagations = reasoner.propagations
    val ers = reasoner.negExistsMapByConcept.getOrElse(ci.superclass, Set.empty)
    for {
      er <- ers
    } {
      newPropagations = (ci.subclass, er) :: newPropagations
      val current = propagations.getOrElse(ci.subclass, Map.empty)
      val newList = er :: current.getOrElse(er.role, Nil)
      propagations = propagations.updated(ci.subclass, current.updated(er.role, newList))
    }
    `R+∃left`(newPropagations, reasoner.copy(propagations = propagations))
  }

  private[this] def `R+∃left`(newPropagations: Iterable[(Concept, ExistentialRestriction)], reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      (concept, er) <- newPropagations
      (role, subjects) <- reasoner.linksByTarget.getOrElse(concept, Map.empty)
      if reasoner.hier.getOrElse(role, Set.empty)(er.role)
      subject <- subjects
    } todo = `Sub+`(ConceptInclusion(subject, er)) :: todo
    reasoner.copy(todo = todo)
  }

  private[this] def `R+∃right`(link: Link, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    val roleToER = reasoner.propagations.getOrElse(link.target, Map.empty)
    for {
      s <- reasoner.hier.getOrElse(link.role, Set.empty)
      f <- roleToER.getOrElse(s, Nil)
    } todo = `Sub+`(ConceptInclusion(link.subject, f)) :: todo
    reasoner.copy(todo = todo)
  }

  private[this] def `R-⟲`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = ci match {
    case ConceptInclusion(sub, SelfRestriction(role)) => reasoner.copy(todo = Link(sub, role, sub) :: reasoner.todo)
    case _                                            => reasoner
  }

  private[this] def `R+⟲`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = ci match {
    case ConceptInclusion(subclass, SelfRestriction(role)) =>
      var todo = reasoner.todo
      for {
        s <- reasoner.hier.getOrElse(role, Set.empty) //TODO this can be propagated ahead of time
        selfRestriction <- reasoner.assertedNegativeSelfRestrictionsByRole.get(s)
      } todo = ConceptInclusion(subclass, selfRestriction) :: todo //TODO could this be Sub+?
      reasoner.copy(todo = todo)
    case _                                                 => reasoner
  }

  private[this] def `R+⟲𝒪`(link: Link, reasoner: ReasonerState): ReasonerState = link match {
    case Link(subject: Nominal, role: Role, target: Nominal) if subject == target =>
      var todo = reasoner.todo
      for {
        s <- reasoner.hier.getOrElse(role, Set.empty) //TODO this can be propagated ahead of time
        selfRestriction <- reasoner.assertedNegativeSelfRestrictionsByRole.get(s)
      } todo = ConceptInclusion(subject, selfRestriction) :: todo //TODO could this be Sub+?
      reasoner.copy(todo = todo)
    case _                                                                        => reasoner
  }

  private[this] def `R∘left`(link: Link, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      (r1, es) <- reasoner.linksByTarget.getOrElse(link.subject, Map.empty)
      r1s = reasoner.hierComps.getOrElse(r1, Map.empty)
      s <- r1s.getOrElse(link.role, Nil)
      e <- es
    } todo = Link(e, s, link.target) :: todo
    reasoner.copy(todo = todo)
  }

  private[this] def `R∘right`(link: Link, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    val r2s = reasoner.hierComps.getOrElse(link.role, Map.empty)
    val linksByLinkSubject = reasoner.linksBySubject(link.subject)
    for {
      (r2, targets) <- reasoner.linksBySubject.getOrElse(link.target, Map.empty)
      s <- r2s.getOrElse(r2, Nil)
      linksWithS = linksByLinkSubject.getOrElse(s, Set.empty)
      d <- targets
      // This is just an optimization to reduce the number of redundant links put on the queue, which can be very large for this rule
      if !linksWithS(d)
    } todo = Link(link.subject, s, d) :: todo
    reasoner.copy(todo = todo)
  }

  private[this] def `R⤳`(link: Link, reasoner: ReasonerState): ReasonerState = reasoner.copy(todo = link.target :: reasoner.todo)

  private[this] def `R⊔`(d: Disjunction): Set[ConceptInclusion] = d.operands.map(o => ConceptInclusion(o, d))

  private[this] def `R¬`(c: Complement): Set[ConceptInclusion] = Set(ConceptInclusion(Conjunction(c.concept, c), Bottom))

  private[this] def saturateRoles(roleInclusions: Set[RoleInclusion]): Map[Role, Set[Role]] = { //FIXME can do this better?
    val subToSuper = roleInclusions.groupBy(_.subproperty).map { case (sub, ri) => sub -> ri.map(_.superproperty) }

    // exclude role avoids infinite loop due to cycles
    def allSupers(role: Role, exclude: Set[Role]): Set[Role] = {
      val currentExclude = exclude + role
      for {
        superProp <- subToSuper.getOrElse(role, Set.empty)
        if !currentExclude(superProp)
        superSuperProp <- allSupers(superProp, currentExclude) + superProp
      } yield superSuperProp
    }

    subToSuper.keys.map(role => role -> allSupers(role, Set.empty)).toMap
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

  trait QueueDelegate {

    def processConcept(concept: Concept, reasoner: ReasonerState): ReasonerState

    def processConceptInclusion(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState

    def processSubPlus(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState

    def processLink(link: Link, reasoner: ReasonerState): ReasonerState

  }

}
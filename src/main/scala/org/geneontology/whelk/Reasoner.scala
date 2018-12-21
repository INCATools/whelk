package org.geneontology.whelk

import BuiltIn._
import scalaz._
import scalaz.Scalaz._
import scala.annotation.tailrec

final case class ReasonerState(
  hier:                                   Map[Role, Set[Role]], // initial
  hierComps:                              Map[Role, Map[Role, List[Role]]], // initial
  assertions:                             List[ConceptInclusion],
  todo:                                   List[QueueExpression],
  topOccursNegatively:                    Boolean,
  inits:                                  Set[Concept], // closure
  assertedConceptInclusionsBySubclass:    Map[Concept, List[ConceptInclusion]],
  closureSubsBySuperclass:                Map[Concept, Set[Concept]],
  closureSubsBySubclass:                  Map[Concept, Set[Concept]],
  assertedNegConjs:                       Set[Conjunction],
  assertedNegConjsByOperandRight:         Map[Concept, List[Conjunction]],
  conjunctionsBySubclassesOfRightOperand: Map[Concept, Map[Concept, Set[Conjunction]]], // Map[subclassOfRightOperand, Map[leftOperand, Conjunction]]
  linksBySubject:                         Map[Concept, Map[Role, Set[Concept]]],
  linksByTarget:                          Map[Concept, Map[Role, List[Concept]]],
  negExistsMapByConcept:                  Map[Concept, Set[ExistentialRestriction]],
  propagations:                           Map[Concept, Map[Role, List[ExistentialRestriction]]],
  ruleEngine:                             RuleEngine,
  wm:                                     WorkingMemory) {

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
    direct(concept, closureSubsBySuperclass.getOrElse(concept, Set.empty) + Bottom, closureSubsBySuperclass.withDefaultValue(Set.empty), Bottom)

  def directlySubsumedBy(concept: Concept): (Set[AtomicConcept], Set[AtomicConcept]) =
    direct(concept, closureSubsBySubclass.getOrElse(concept, Set.empty) + Top, closureSubsBySubclass.withDefaultValue(Set.empty), Top)

  //  def directSubsumes(concept: Concept, allSubsConcepts: Set[Concept]): (Set[AtomicConcept], Set[AtomicConcept]) =
  //    direct(concept, allSubsConcepts + Bottom, closureSubsBySuperclass.withDefaultValue(Set.empty))

  def directSubsumers(concept: Concept, allSubsConcepts: Set[Concept]): (Set[AtomicConcept], Set[AtomicConcept]) =
    direct(concept, allSubsConcepts + Top, closureSubsBySubclass.withDefaultValue(Set.empty), Top)

  def direct(concept: Concept, allSubsConcepts: Set[Concept], subsumerFunction: Concept => Concept => Boolean, tautology: Concept): (Set[AtomicConcept], Set[AtomicConcept]) = {
    val allSubsumers = allSubsConcepts.iterator.collect { case ac: AtomicConcept => ac }.filterNot(_ == concept)
    allSubsumers.foldLeft((Set.empty[AtomicConcept], Set.empty[AtomicConcept])) {
      case ((equivalents, directSubsumers), subsumer) =>
        if (concept == tautology || subsumerFunction(subsumer)(concept)) (equivalents + subsumer, directSubsumers)
        else {
          @tailrec
          def loop(currentDirectSubs: List[AtomicConcept], isDirect: Boolean, remove: List[AtomicConcept], add: List[AtomicConcept]): (List[AtomicConcept], List[AtomicConcept]) =
            (currentDirectSubs, isDirect, remove, add) match {
              case (Nil, true, removes, adds) => (removes, subsumer :: adds)
              case (Nil, false, removes, adds) => (removes, adds)
              case (other :: _, _, removes, adds) if subsumer == tautology || subsumerFunction(other)(subsumer) => (removes, adds)
              case (other :: rest, direct, removes, adds) if other == tautology || subsumerFunction(subsumer)(other) => loop(rest, direct, other :: removes, adds)
              case (_ :: rest, direct, removes, adds) => loop(rest, direct, removes, adds)
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
    import scalaz.syntax.semigroup._
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
  private def computeClosure(reasoner: ReasonerState): ReasonerState = {
    if (reasoner.assertions.nonEmpty) {
      val item :: todoAssertions = reasoner.assertions
      computeClosure(processAssertedConceptInclusion(item, reasoner.copy(assertions = todoAssertions)))
    } else if (reasoner.todo.nonEmpty) {
      val item :: todo = reasoner.todo
      computeClosure(process(item, reasoner.copy(todo = todo)))
    } else reasoner
  }

  private def processAssertedConceptInclusion(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val updated = reasoner.assertedConceptInclusionsBySubclass.updated(ci.subclass, ci :: reasoner.assertedConceptInclusionsBySubclass.getOrElse(ci.subclass, Nil))
    `R⊑left`(ci, `R+∃a`(ci, `R+⨅a`(ci, `R⊤left`(ci, reasoner.copy(assertedConceptInclusionsBySubclass = updated)))))
  }

  private def process(expression: QueueExpression, reasoner: ReasonerState): ReasonerState = {
    expression match {
      case concept: Concept             => processConcept(concept, reasoner)
      case ci: ConceptInclusion         => processConceptInclusion(ci, reasoner)
      case `Sub+`(ci: ConceptInclusion) => processSubPlus(ci, reasoner)
      case link: Link                   => processLink(link, reasoner)
    }
  }

  private def processConcept(concept: Concept, reasoner: ReasonerState): ReasonerState = {
    if (reasoner.inits(concept)) reasoner else
      `R⊤right`(concept, R0(concept, reasoner.copy(inits = reasoner.inits + concept)))
  }

  private def processConceptInclusion(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val ConceptInclusion(subclass, superclass) = ci
    val subs = reasoner.closureSubsBySuperclass.getOrElse(superclass, Set.empty)
    if (subs(subclass)) reasoner else {
      val closureSubsBySuperclass = reasoner.closureSubsBySuperclass.updated(superclass, subs + subclass)
      val supers = reasoner.closureSubsBySubclass.getOrElse(subclass, Set.empty)
      val closureSubsBySubclass = reasoner.closureSubsBySubclass.updated(subclass, supers + superclass)
      val updatedReasoner = `R⊑right`(ci, `R+∃b-right`(ci, `R-∃`(ci, `R+⨅b-right`(ci, `R+⨅right`(ci, `R-⨅`(ci, `R⊥left`(ci, reasoner.copy(closureSubsBySuperclass = closureSubsBySuperclass, closureSubsBySubclass = closureSubsBySubclass))))))))
      ci match {
        case ConceptInclusion(Nominal(ind), concept) => reasoner.ruleEngine.processConceptAssertion(ConceptAssertion(concept, ind), updatedReasoner)
        case _ => updatedReasoner
      }
    }
  }

  private def processSubPlus(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val ConceptInclusion(subclass, superclass) = ci
    val subs = reasoner.closureSubsBySuperclass.getOrElse(superclass, Set.empty)
    if (subs(subclass)) reasoner else {
      val closureSubsBySuperclass = reasoner.closureSubsBySuperclass.updated(superclass, subs + subclass)
      val supers = reasoner.closureSubsBySubclass.getOrElse(subclass, Set.empty)
      val closureSubsBySubclass = reasoner.closureSubsBySubclass.updated(subclass, supers + superclass)
      val updatedReasoner = `R⊑right`(ci, `R+∃b-right`(ci, `R+⨅b-right`(ci, `R+⨅right`(ci, `R⊥left`(ci, reasoner.copy(closureSubsBySuperclass = closureSubsBySuperclass, closureSubsBySubclass = closureSubsBySubclass))))))
      ci match {
        case ConceptInclusion(Nominal(ind), concept) => updatedReasoner.ruleEngine.processConceptAssertion(ConceptAssertion(concept, ind), updatedReasoner)
        case _ => updatedReasoner
      }
    }
  }

  private def processLink(link: Link, reasoner: ReasonerState): ReasonerState = {
    val Link(subject, role, target) = link
    val rolesToTargets = reasoner.linksBySubject.getOrElse(subject, Map.empty)
    val targetsSet = rolesToTargets.getOrElse(role, Set.empty[Concept])
    if (targetsSet(target)) reasoner else {
      val updatedTargetsSet = targetsSet + target
      val updatedRolesToTargets = rolesToTargets.updated(link.role, updatedTargetsSet)
      val linksBySubject = reasoner.linksBySubject.updated(subject, updatedRolesToTargets)
      val rolesToSubjects = reasoner.linksByTarget.getOrElse(target, Map.empty)
      val subjects = rolesToSubjects.getOrElse(role, Nil)
      val updatedSubjects = subject :: subjects
      val updatedRolesToSubjects = rolesToSubjects.updated(role, updatedSubjects)
      val linksByTarget = reasoner.linksByTarget.updated(target, updatedRolesToSubjects)
      val updatedReasoner = `R⤳`(link, `R∘left`(link, `R∘right`(link, `R+∃right`(link, `R⊥right`(link, reasoner.copy(linksBySubject = linksBySubject, linksByTarget = linksByTarget))))))
      link match {
        case Link(Nominal(subjectInd), aRole, Nominal(targetInd)) => updatedReasoner.ruleEngine.processRoleAssertion(RoleAssertion(aRole, subjectInd, targetInd), updatedReasoner)
        case _ => updatedReasoner
      }
    }
  }

  private def R0(concept: Concept, reasoner: ReasonerState): ReasonerState =
    reasoner.copy(todo = ConceptInclusion(concept, concept) :: reasoner.todo)

  private def `R⊤left`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState =
    if (ci.subclass.signature(Top)) reasoner.copy(topOccursNegatively = true) else reasoner

  private def `R⊤right`(concept: Concept, reasoner: ReasonerState): ReasonerState =
    if (reasoner.topOccursNegatively) reasoner.copy(todo = ConceptInclusion(concept, Top) :: reasoner.todo)
    else reasoner

  private def `R⊑left`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    reasoner.closureSubsBySuperclass.getOrElse(ci.subclass, Set.empty).foreach { other =>
      todo = ConceptInclusion(other, ci.superclass) :: todo
    }
    reasoner.copy(todo = todo)
  }

  private def `R⊑right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    reasoner.assertedConceptInclusionsBySubclass.getOrElse(ci.superclass, Nil).foreach { other =>
      todo = ConceptInclusion(ci.subclass, other.superclass) :: todo
    }
    reasoner.copy(todo = todo)
  }

  private def `R⊥left`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    if (ci.superclass == Bottom) {
      for {
        (_, subjects) <- reasoner.linksByTarget.getOrElse(ci.subclass, Map.empty)
        subject <- subjects
      } todo = ConceptInclusion(subject, Bottom) :: todo
      reasoner.copy(todo = todo)
    } else reasoner
  }

  private def `R⊥right`(link: Link, reasoner: ReasonerState): ReasonerState = {
    if (reasoner.closureSubsBySuperclass(Bottom)(link.target))
      reasoner.copy(todo = ConceptInclusion(link.subject, Bottom) :: reasoner.todo)
    else reasoner
  }

  private def `R-⨅`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = ci match {
    case ConceptInclusion(sub, Conjunction(left, right)) => reasoner.copy(todo = ConceptInclusion(sub, left) :: ConceptInclusion(sub, right) :: reasoner.todo)
    case _ => reasoner
  }

  private def `R+⨅a`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val newNegativeConjunctions = ci.subclass.conceptSignature.collect { case conj: Conjunction => conj }.filterNot(reasoner.assertedNegConjs)
    val newAssertedNegConjs = reasoner.assertedNegConjs ++ newNegativeConjunctions
    val newNegConjsByOperandRight = newNegativeConjunctions.foldLeft(reasoner.assertedNegConjsByOperandRight) {
      case (acc, c @ Conjunction(_, right)) =>
        val updated = c :: acc.getOrElse(right, Nil)
        acc.updated(right, updated)
    }
    `R+⨅b-left`(newNegativeConjunctions, reasoner.copy(assertedNegConjs = newAssertedNegConjs, assertedNegConjsByOperandRight = newNegConjsByOperandRight))
  }

  private def `R+⨅b-left`(newNegativeConjunctions: Iterable[Conjunction], reasoner: ReasonerState): ReasonerState = {
    var conjunctionsBySubclassesOfRightOperand = reasoner.conjunctionsBySubclassesOfRightOperand
    var newSubclassesAndConjunctions: List[(Concept, Conjunction)] = Nil
    for {
      conjunction <- newNegativeConjunctions
      cs <- reasoner.closureSubsBySuperclass.get(conjunction.right).toIterable
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

  private def `R+⨅b-right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var conjunctionsBySubclassesOfRightOperand = reasoner.conjunctionsBySubclassesOfRightOperand
    var newSubclassesAndConjunctions: List[(Concept, Conjunction)] = Nil
    for {
      conjunctions <- reasoner.assertedNegConjsByOperandRight.get(ci.superclass).toIterable
      conjunction <- conjunctions
    } {
      newSubclassesAndConjunctions = (ci.subclass -> conjunction) :: newSubclassesAndConjunctions
      val conjunctionsByLeft = conjunctionsBySubclassesOfRightOperand.getOrElse(ci.subclass, Map.empty)
      val newConjunctionsForThisLeft = conjunctionsByLeft.getOrElse(conjunction.left, Set.empty) + conjunction
      val newValue = conjunctionsByLeft.updated(conjunction.left, newConjunctionsForThisLeft)
      conjunctionsBySubclassesOfRightOperand = conjunctionsBySubclassesOfRightOperand.updated(ci.subclass, newValue)
    }
    `R+⨅left`(newSubclassesAndConjunctions, reasoner.copy(conjunctionsBySubclassesOfRightOperand = conjunctionsBySubclassesOfRightOperand))
  }

  private def `R+⨅left`(subclassesAndConjunctions: Iterable[(Concept, Conjunction)], reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      (c, conjunction) <- subclassesAndConjunctions
      subclasses <- reasoner.closureSubsBySuperclass.get(conjunction.left)
      if subclasses(c)
    } todo = `Sub+`(ConceptInclusion(c, conjunction)) :: todo
    reasoner.copy(todo = todo)
  }

  private def `R+⨅right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      conjunctionsByLeft <- reasoner.conjunctionsBySubclassesOfRightOperand.get(ci.subclass)
      conjunctions <- conjunctionsByLeft.get(ci.superclass)
      conjunction <- conjunctions
    } todo = `Sub+`(ConceptInclusion(ci.subclass, conjunction)) :: todo
    reasoner.copy(todo = todo)
  }

  private def `R-∃`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = ci match {
    case ConceptInclusion(c, ExistentialRestriction(role, filler)) => reasoner.copy(todo = Link(c, role, filler) :: reasoner.todo)
    case _ => reasoner
  }

  private def `R+∃a`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val newNegativeExistentials = ci.subclass.conceptSignature.collect { case er: ExistentialRestriction => er }
    val negExistsMapByConcept = newNegativeExistentials.foldLeft(reasoner.negExistsMapByConcept) { (acc, er) =>
      val updated = acc.getOrElse(er.concept, Set.empty) + er
      acc.updated(er.concept, updated)
    }
    `R+∃b-left`(newNegativeExistentials, reasoner.copy(negExistsMapByConcept = negExistsMapByConcept))
  }

  private def `R+∃b-left`(newNegativeExistentials: Iterable[ExistentialRestriction], reasoner: ReasonerState): ReasonerState = {
    var newPropagations: List[(Concept, ExistentialRestriction)] = Nil
    var propagations = reasoner.propagations
    for {
      er <- newNegativeExistentials
      subclasses <- reasoner.closureSubsBySuperclass.get(er.concept).toIterable
      subclass <- subclasses
    } {
      newPropagations = (subclass, er) :: newPropagations
      val current = propagations.getOrElse(subclass, Map.empty)
      val newList = er :: current.getOrElse(er.role, Nil)
      propagations = propagations.updated(subclass, current.updated(er.role, newList))
    }
    `R+∃left`(newPropagations, reasoner.copy(propagations = propagations))
  }

  private def `R+∃b-right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = { //speed up
    var newPropagations: List[(Concept, ExistentialRestriction)] = Nil
    var propagations = reasoner.propagations
    for {
      ers <- reasoner.negExistsMapByConcept.get(ci.superclass).toIterable
      er <- ers
    } {
      newPropagations = (ci.subclass, er) :: newPropagations
      val current = propagations.getOrElse(ci.subclass, Map.empty)
      val newList = er :: current.getOrElse(er.role, Nil)
      propagations = propagations.updated(ci.subclass, current.updated(er.role, newList))
    }
    `R+∃left`(newPropagations, reasoner.copy(propagations = propagations))
  }

  private def `R+∃left`(newPropagations: Iterable[(Concept, ExistentialRestriction)], reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      (concept, er) <- newPropagations
      (role, subjects) <- reasoner.linksByTarget.getOrElse(concept, Map.empty)
      // This getOrElse could be removed if new roles are added to hier anytime assertions are added
      if reasoner.hier.getOrElse(role, Set.empty)(er.role)
      subject <- subjects
    } todo = `Sub+`(ConceptInclusion(subject, er)) :: todo
    reasoner.copy(todo = todo)
  }

  private def `R+∃right`(link: Link, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      roleToER <- reasoner.propagations.get(link.target).toIterable
      s <- reasoner.hier.getOrElse(link.role, Set.empty)
      fs <- roleToER.get(s)
      f <- fs
    } todo = `Sub+`(ConceptInclusion(link.subject, f)) :: todo
    reasoner.copy(todo = todo)
  }

  private def `R∘left`(link: Link, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      (r1, es) <- reasoner.linksByTarget.getOrElse(link.subject, Map.empty)
      r1s <- reasoner.hierComps.get(r1)
      ss <- r1s.get(link.role)
      s <- ss
      e <- es
    } todo = Link(e, s, link.target) :: todo
    reasoner.copy(todo = todo)
  }

  private def `R∘right`(link: Link, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    val r2s = reasoner.hierComps.getOrElse(link.role, Map.empty)
    for {
      (r2, targets) <- reasoner.linksBySubject.getOrElse(link.target, Map.empty)
      ss <- r2s.get(r2)
      s <- ss
      d <- targets
    } todo = Link(link.subject, s, d) :: todo
    reasoner.copy(todo = todo)
  }

  private def `R⤳`(link: Link, reasoner: ReasonerState): ReasonerState = reasoner.copy(todo = link.target :: reasoner.todo)

  private def `R⊔`(d: Disjunction): Set[ConceptInclusion] = d.operands.map(o => ConceptInclusion(o, d))

  private def `R¬`(c: Complement): Set[ConceptInclusion] = Set(ConceptInclusion(Conjunction(c.concept, c), Bottom))

  private def saturateRoles(roleInclusions: Set[RoleInclusion]): Map[Role, Set[Role]] = { //FIXME can do this better?
    val subToSuper = roleInclusions.groupBy(_.subproperty).map { case (sub, ri) => sub -> ri.map(_.superproperty) }
    def allSupers(role: Role): Set[Role] = for {
      superProp <- subToSuper.getOrElse(role, Set.empty)
      superSuperProp <- allSupers(superProp) + superProp
    } yield superSuperProp
    subToSuper.keys.map(role => role -> allSupers(role)).toMap
  }

  private def indexRoleCompositions(hier: Map[Role, Set[Role]], chains: Set[RoleComposition]): Map[Role, Map[Role, List[Role]]] = {
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
package org.geneontology.whelk

import scala.collection.immutable.Queue

import BuiltIn._
import scalaz._
import scalaz.Scalaz._

final case class ReasonerState(
  hier:                                  Map[Role, Set[Role]], // initial
  hierComps:                             Map[Role, Map[Role, Set[Role]]], // initial
  assertions:                            Queue[ConceptInclusion],
  todo:                                  Queue[QueueExpression],
  topOccursNegatively:                   Boolean,
  inits:                                 Set[Concept], // closure
  assertedConceptInclusionsBySubclass:   Map[Concept, List[ConceptInclusion]],
  closureSubsBySuperclass:               Map[Concept, Set[Concept]],
  assertedNegConjsByOperandLeft:         Map[Concept, Set[Conjunction]],
  conjunctionsBySubclassesOfLeftOperand: Map[Concept, Map[Concept, Set[Conjunction]]], // Map[subclassOfLeftOperand, Map[rightOperand, Conjunction]]
  linksBySubject:                        Map[Concept, Set[Link]],
  linksByTarget:                         Map[Concept, List[Link]],
  negExistsMapByConcept:                 Map[Concept, Set[ExistentialRestriction]],
  propagations:                          Map[Concept, Map[Role, List[ExistentialRestriction]]]) {

  def subs: Set[ConceptInclusion] = closureSubsBySuperclass.flatMap {
    case (superclass, subclasses) =>
      subclasses.map(ConceptInclusion(_, superclass))
  }.toSet

}

object ReasonerState {

  val empty: ReasonerState = ReasonerState(Map.empty, Map.empty, Queue.empty, Queue.empty, false, Set.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

}

object Reasoner {

  def assert(axioms: Set[Axiom]): ReasonerState = {
    import scalaz.syntax.semigroup._
    val allRoles = axioms.flatMap(_.signature).collect { case role: Role => role }
    val allRoleInclusions = axioms.collect { case ri: RoleInclusion => ri }
    val hier: Map[Role, Set[Role]] = saturateRoles(allRoleInclusions) |+| allRoles.map(r => r -> Set(r)).toMap
    val hierComps = indexRoleCompositions(hier, axioms.collect { case rc: RoleComposition => rc })
    val concIncs = axioms.collect { case ci: ConceptInclusion => ci }
    assert(concIncs, ReasonerState.empty.copy(hier = hier, hierComps = hierComps))
  }

  def assert(axioms: Set[ConceptInclusion], reasoner: ReasonerState): ReasonerState = {
    computeClosure(reasoner.copy(
      assertions = reasoner.assertions.enqueue(axioms),
      todo = reasoner.todo.enqueue(axioms)))
  }

  private def computeClosure(reasoner: ReasonerState): ReasonerState = {
    if (reasoner.assertions.nonEmpty) {
      val (item, todoAssertions) = reasoner.assertions.dequeue
      computeClosure(processAssertedConceptInclusion(item, reasoner.copy(assertions = todoAssertions)))
    } else if (reasoner.todo.nonEmpty) {
      val (item, todo) = reasoner.todo.dequeue
      computeClosure(process(item, reasoner.copy(todo = todo)))
    } else reasoner
  }

  private def processAssertedConceptInclusion(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val updated = reasoner.assertedConceptInclusionsBySubclass + (ci.subclass -> (ci :: reasoner.assertedConceptInclusionsBySubclass.getOrElse(ci.subclass, Nil)))
    `R⊑left`(ci, `R+∃a`(ci, `R+⨅a`(ci, `R⊤left`(ci, reasoner.copy(assertedConceptInclusionsBySubclass = updated)))))
  }

  private def process(expression: QueueExpression, reasoner: ReasonerState): ReasonerState = {
    expression match {
      case concept: Concept => if (reasoner.inits(concept)) reasoner else
        `R⊤right`(concept, R0(concept, reasoner.copy(inits = reasoner.inits + concept)))
      case ci @ ConceptInclusion(subclass, superclass) =>
        val subs = reasoner.closureSubsBySuperclass.getOrElse(superclass, Set.empty)
        if (subs(subclass)) reasoner else {
          val closureSubsBySuperclass = reasoner.closureSubsBySuperclass + (superclass -> (subs + subclass))
          `R⊑right`(ci, `R+∃b-right`(ci, `R-∃`(ci, `R+⨅b-right`(ci, `R+⨅right`(ci, `R-⨅`(ci, `R⊥left`(ci, reasoner.copy(closureSubsBySuperclass = closureSubsBySuperclass))))))))
        }
      case `Sub+`(ci @ ConceptInclusion(subclass, superclass)) =>
        val subs = reasoner.closureSubsBySuperclass.getOrElse(superclass, Set.empty)
        if (subs(subclass)) reasoner else {
          val closureSubsBySuperclass = reasoner.closureSubsBySuperclass + (superclass -> (subs + subclass))
          `R⊑right`(ci, `R+∃b-right`(ci, `R+⨅b-right`(ci, `R+⨅right`(ci, `R⊥left`(ci, reasoner.copy(closureSubsBySuperclass = closureSubsBySuperclass))))))
        }
      case link @ Link(subject, role, target) =>
        val links = reasoner.linksBySubject.getOrElse(subject, Set.empty)
        if (links(link)) reasoner else {
          val updatedLinks = links + link
          val linksBySubject = reasoner.linksBySubject + (subject -> updatedLinks)
          val linksByTarget = reasoner.linksByTarget + (target -> (link :: reasoner.linksByTarget.getOrElse(target, Nil)))
          `R⤳`(link, `R∘left`(link, `R∘right`(link, `R+∃right`(link, `R⊥right`(link, reasoner.copy(linksBySubject = linksBySubject, linksByTarget = linksByTarget))))))
        }
    }
  }

  private def R0(concept: Concept, reasoner: ReasonerState): ReasonerState =
    reasoner.copy(todo = reasoner.todo.enqueue(ConceptInclusion(concept, concept)))

  private def `R⊤left`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState =
    if (ci.subclass.signature(Top)) reasoner.copy(topOccursNegatively = true) else reasoner

  private def `R⊤right`(concept: Concept, reasoner: ReasonerState): ReasonerState =
    if (reasoner.topOccursNegatively) reasoner.copy(todo = reasoner.todo.enqueue(ConceptInclusion(concept, Top)))
    else reasoner

  private def `R⊑left`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    reasoner.closureSubsBySuperclass.getOrElse(ci.subclass, Set.empty).foreach { other =>
      todo = todo.enqueue(ConceptInclusion(other, ci.superclass))
    }
    reasoner.copy(todo = todo)
  }

  private def `R⊑right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    reasoner.assertedConceptInclusionsBySubclass.getOrElse(ci.superclass, Nil).foreach { other =>
      todo = todo.enqueue(ConceptInclusion(ci.subclass, other.superclass))
    }
    reasoner.copy(todo = todo)
  }

  private def `R⊥left`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    if (ci.superclass == Bottom) {
      reasoner.linksByTarget.getOrElse(ci.subclass, Nil).foreach { link =>
        todo = todo.enqueue(ConceptInclusion(link.subject, Bottom))
      }
      reasoner.copy(todo = todo)
    } else reasoner
  }

  private def `R⊥right`(link: Link, reasoner: ReasonerState): ReasonerState = {
    if (reasoner.closureSubsBySuperclass.getOrElse(Bottom, Set.empty)(link.target))
      reasoner.copy(todo = reasoner.todo.enqueue(ConceptInclusion(link.subject, Bottom)))
    else reasoner
  }

  private def `R-⨅`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = ci match {
    case ConceptInclusion(sub, Conjunction(left, right)) => reasoner.copy(todo = reasoner.todo
      .enqueue(ConceptInclusion(sub, left))
      .enqueue(ConceptInclusion(sub, right)))
    case _ => reasoner
  }

  private def `R+⨅a`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val newNegativeConjunctions = ci.subclass.conceptSignature.collect { case conj: Conjunction => conj }
    val newNegConjsByOperandLeft = newNegativeConjunctions.foldLeft(reasoner.assertedNegConjsByOperandLeft) {
      case (acc, c @ Conjunction(left, right)) =>
        val updated = acc.getOrElse(left, Set.empty) + c
        acc + (left -> updated)
    }
    `R+⨅b-left`(newNegativeConjunctions, reasoner.copy(assertedNegConjsByOperandLeft = newNegConjsByOperandLeft))
  }

  private def mergeConjunctionIndexes(existing: Map[Concept, Map[Concept, Set[Conjunction]]], toAdd: Iterable[(Concept, Conjunction)]): Map[Concept, Map[Concept, Set[Conjunction]]] =
    toAdd.foldLeft(existing) {
      case (acc, (concept, conjunction)) =>
        val conjunctionsByRight = acc.getOrElse(concept, Map.empty)
        val newConjunctionsForThisRight = conjunctionsByRight.getOrElse(conjunction.right, Set.empty) + conjunction
        val newValue = conjunctionsByRight + (conjunction.right -> newConjunctionsForThisRight)
        acc + (concept -> newValue)
    }

  private def `R+⨅b-left`(newNegativeConjunctions: Iterable[Conjunction], reasoner: ReasonerState): ReasonerState = {
    val newSubclassesAndConjunctions = for {
      conjunction <- newNegativeConjunctions
      cs <- reasoner.closureSubsBySuperclass.get(conjunction.left).toIterable
      c <- cs
    } yield c -> conjunction
    val newIndex = mergeConjunctionIndexes(reasoner.conjunctionsBySubclassesOfLeftOperand, newSubclassesAndConjunctions)
    `R+⨅left`(newSubclassesAndConjunctions, reasoner.copy(conjunctionsBySubclassesOfLeftOperand = newIndex))
  }

  private def `R+⨅b-right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val newSubclassesAndConjunctions = for {
      conjunctions <- reasoner.assertedNegConjsByOperandLeft.get(ci.superclass).toIterable
      conjunction <- conjunctions
    } yield ci.subclass -> conjunction
    val newIndex = mergeConjunctionIndexes(reasoner.conjunctionsBySubclassesOfLeftOperand, newSubclassesAndConjunctions)
    `R+⨅left`(newSubclassesAndConjunctions, reasoner.copy(conjunctionsBySubclassesOfLeftOperand = newIndex))
  }

  private def `R+⨅left`(subclassesAndConjunctions: Iterable[(Concept, Conjunction)], reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      (c, conjunction) <- subclassesAndConjunctions
      subclasses <- reasoner.closureSubsBySuperclass.get(conjunction.right)
      if subclasses(c)
    } todo = todo.enqueue(`Sub+`(ConceptInclusion(c, conjunction)))
    reasoner.copy(todo = todo)
  }

  private def `R+⨅right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      conjunctionsByRight <- reasoner.conjunctionsBySubclassesOfLeftOperand.get(ci.subclass)
      conjunctions <- conjunctionsByRight.get(ci.superclass)
      conjunction <- conjunctions
    } todo = todo.enqueue(`Sub+`(ConceptInclusion(ci.subclass, conjunction)))
    reasoner.copy(todo = todo)
  }

  private def `R-∃`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = ci match {
    case ConceptInclusion(c, ExistentialRestriction(role, filler)) => reasoner.copy(todo = reasoner.todo
      .enqueue(Link(c, role, filler)))
    case _ => reasoner
  }

  private def `R+∃a`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val newNegativeExistentials = ci.subclass.conceptSignature.collect { case er: ExistentialRestriction => er }
    val negExistsMapByConcept = newNegativeExistentials.foldLeft(reasoner.negExistsMapByConcept) { (acc, er) =>
      val updated = acc.getOrElse(er.concept, Set.empty) + er
      acc + (er.concept -> updated)
    }
    `R+∃b-left`(newNegativeExistentials, reasoner.copy(negExistsMapByConcept = negExistsMapByConcept))
  }

  private def `R+∃b-left`(newNegativeExistentials: Iterable[ExistentialRestriction], reasoner: ReasonerState): ReasonerState = {
    val newPropagations = for {
      er <- newNegativeExistentials
      subclasses <- reasoner.closureSubsBySuperclass.get(er.concept).toIterable
      subclass <- subclasses
    } yield subclass -> er
    val propagations = newPropagations.foldLeft(reasoner.propagations) {
      case (acc, (concept, er)) =>
        val current = acc.getOrElse(er.concept, Map.empty)
        val newList = er :: current.getOrElse(er.role, Nil)
        acc + (er.concept -> (current + (er.role -> newList)))
    }
    `R+∃left`(newPropagations, reasoner.copy(propagations = propagations))
  }

  private def `R+∃b-right`(ci: ConceptInclusion, reasoner: ReasonerState): ReasonerState = {
    val newPropagations = for {
      ers <- reasoner.negExistsMapByConcept.get(ci.superclass).toIterable
      er <- ers
    } yield ci.subclass -> er
    val propagations = newPropagations.foldLeft(reasoner.propagations) {
      case (acc, (concept, er)) =>
        val current = acc.getOrElse(concept, Map.empty)
        val newList = er :: current.getOrElse(er.role, Nil)
        acc + (concept -> (current + (er.role -> newList)))
    }
    `R+∃left`(newPropagations, reasoner.copy(propagations = propagations))
  }

  private def `R+∃left`(newPropagations: Iterable[(Concept, ExistentialRestriction)], reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      (concept, er) <- newPropagations
      links <- reasoner.linksByTarget.get(concept)
      link <- links
      if reasoner.hier(link.role)(er.role)
    } todo = todo.enqueue(`Sub+`(ConceptInclusion(link.subject, er)))
    reasoner.copy(todo = todo)
  }

  private def `R+∃right`(link: Link, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      roleToER <- reasoner.propagations.get(link.target).toIterable
      s <- reasoner.hier(link.role)
      fs <- roleToER.get(s)
      f <- fs
    } todo = todo.enqueue(`Sub+`(ConceptInclusion(link.subject, f)))
    reasoner.copy(todo = todo)
  }

  private def `R∘left`(link: Link, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      Link(e, r1, c) <- reasoner.linksByTarget.getOrElse(link.subject, Nil)
      r1s <- reasoner.hierComps.get(r1)
      ss <- r1s.get(link.role)
      s <- ss
    } todo = todo.enqueue(Link(e, s, link.target))
    reasoner.copy(todo = todo)
  }

  private def `R∘right`(link: Link, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      Link(_, r2, d) <- reasoner.linksBySubject.getOrElse(link.target, Set.empty)
      r2s <- reasoner.hierComps.get(link.role)
      ss <- r2s.get(r2)
      s <- ss
    } todo = todo.enqueue(Link(link.subject, s, d))
    reasoner.copy(todo = todo)
  }

  private def `R⤳`(link: Link, reasoner: ReasonerState): ReasonerState = reasoner.copy(todo = reasoner.todo.enqueue(link.target))

  private def saturateRoles(roleInclusions: Set[RoleInclusion]): Map[Role, Set[Role]] = { //FIXME can do this better?
    val subToSuper = roleInclusions.groupBy(_.subproperty).map { case (sub, ri) => sub -> ri.map(_.superproperty) }
    def allSupers(role: Role): Set[Role] = for {
      superProp <- subToSuper.getOrElse(role, Set.empty)
      superSuperProp <- allSupers(superProp) + superProp
    } yield superSuperProp
    subToSuper.keys.map(role => role -> allSupers(role)).toMap
  }

  private def indexRoleCompositions(hier: Map[Role, Set[Role]], chains: Set[RoleComposition]): Map[Role, Map[Role, Set[Role]]] = {
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
      case (r1, values) => r1 -> (values.map {
        case (r1, r2, s) => (r2, s)
      }).groupBy(_._1).map {
        case (r2, ss) => r2 -> ss.map(_._2)
      }
    }
    hierComps
  }

}
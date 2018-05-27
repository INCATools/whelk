package org.geneontology.whelk

import scala.annotation.tailrec
import scala.collection.immutable.Queue

final case class Reasoner(
  todo:                 Queue[QueueExpression], // need to initialize from ont
  concIncs:             Set[ConceptInclusion], // based on ont
  concIncsBySuperclass: Map[Concept, List[ConceptInclusion]],
  inits:                Set[Concept], // closure
  subs:                 Set[ConceptInclusion], // closure
  subsBySubclass:       Map[Concept, List[ConceptInclusion]],
  links:                Set[Link], // closure
  linksBySubject:       Map[Concept, List[Link]],
  linksByTarget:        Map[Concept, List[Link]],
  negConjs:             Set[Conjunction], // based on ont
  negConjsByOperand:    Map[Concept, List[Conjunction]], //NOT USED
  negExists:            Set[ExistentialRestriction], // based on ont
  hier:                 Map[Role, Set[Role]], // based on ont
  roleComps:            Map[(Role, Role), Set[Role]], // based on ont
  topOccursNegatively:  Boolean) // based on ont

object Reasoner {

  val empty: Reasoner = Reasoner(Queue.empty, Set.empty, Map.empty, Set.empty, Set.empty, Map.empty, Set.empty, Map.empty, Map.empty, Set.empty, Map.empty, Set.empty, Map.empty, Map.empty, false)

  def prepare(axioms: Set[Axiom]): Reasoner = {
    val concIncs = axioms.collect { case ci: ConceptInclusion => ci }
    val concIncsBySuperclass = concIncs.groupBy(_.superclass).mapValues(_.toList)
    val todo = Queue.empty.enqueue(concIncs)
    val negativeConcepts = concIncs.flatMap(_.subclass.conceptSignature)
    val negConjs = negativeConcepts.collect { case conj: Conjunction => conj }
    val negExists = negativeConcepts.collect { case er: ExistentialRestriction => er }
    empty.copy(todo = todo, concIncs = concIncs, concIncsBySuperclass = concIncsBySuperclass, negConjs = negConjs, negExists = negExists, topOccursNegatively = negativeConcepts(Top))
  }

  @tailrec
  def computeClosure(reasoner: Reasoner): Reasoner = if (reasoner.todo.nonEmpty) {
    val (item, todo) = reasoner.todo.dequeue
    computeClosure(process(item, reasoner.copy(todo = todo)))
  } else reasoner

  private def process(expression: QueueExpression, reasoner: Reasoner): Reasoner = expression match {
    case concept: Concept => if (reasoner.inits(concept)) reasoner else
      `R⊤`(concept, R0(concept, reasoner.copy(inits = reasoner.inits + concept)))
    case ci @ ConceptInclusion(subclass, superclass) => if (reasoner.subs(ci)) reasoner else {
      val subs = reasoner.subs + ci
      val subsBySubclass = reasoner.subsBySubclass + (ci.subclass -> (ci :: reasoner.subsBySubclass.getOrElse(ci.subclass, Nil)))
      `R⊑`(ci, `R+∃`(ci, `R-∃`(ci, `R+⨅`(ci, `R-⨅`(ci, `R⊥`(ci, reasoner.copy(subs = subs, subsBySubclass = subsBySubclass)))))))
    }
    case link @ Link(subclass, role, superclass) => if (reasoner.links(link)) reasoner else {
      val links = reasoner.links + link
      val linksBySubject = reasoner.linksBySubject + (link.subject -> (link :: reasoner.linksBySubject.getOrElse(link.subject, Nil)))
      val linksByTarget = reasoner.linksByTarget + (link.target -> (link :: reasoner.linksByTarget.getOrElse(link.target, Nil)))
      `R⤳`(link, `R∘`(link, `R+∃`(link, `R⊥`(link, reasoner.copy(links = links, linksBySubject = linksBySubject, linksByTarget = linksByTarget)))))
    }
  }

  private def R0(concept: Concept, reasoner: Reasoner): Reasoner =
    reasoner.copy(todo = reasoner.todo.enqueue(ConceptInclusion(concept, concept)))

  private def `R⊤`(concept: Concept, reasoner: Reasoner): Reasoner =
    if (reasoner.topOccursNegatively) reasoner.copy(todo = reasoner.todo.enqueue(ConceptInclusion(concept, Top)))
    else reasoner

  // get from links indexed by target, map over collection of links to get their subjects
  private def `R⊥`(ci: ConceptInclusion, reasoner: Reasoner): Reasoner =
    if (ci.superclass == Bottom) reasoner.copy(todo = reasoner.todo
      .enqueue(reasoner.links.collect { case Link(e, r, ci.subclass) => ConceptInclusion(e, Bottom) }))
    else reasoner

  private def `R-⨅`(ci: ConceptInclusion, reasoner: Reasoner): Reasoner = ci match {
    case ConceptInclusion(sub, Conjunction(left, right)) => reasoner.copy(todo = reasoner.todo
      .enqueue(ConceptInclusion(sub, left))
      .enqueue(ConceptInclusion(sub, right)))
    case _ => reasoner
  }

  // get from subs indexed by subclass; get from negConjs indexed by either operand
  private def `R+⨅`(ci: ConceptInclusion, reasoner: Reasoner): Reasoner = reasoner.copy(todo = reasoner.todo.enqueue(
    reasoner.subs.collect {
      case ConceptInclusion(ci.subclass, other) =>
        reasoner.negConjs.filter { conj =>
          ((conj.left == ci.superclass) && (conj.right == other)) || ((conj.left == other) && (conj.right == ci.superclass))
        }.map(ConceptInclusion(ci.subclass, _))
    }.flatten))

  private def `R-∃`(ci: ConceptInclusion, reasoner: Reasoner): Reasoner = ci match {
    case ConceptInclusion(c, ExistentialRestriction(role, filler)) => reasoner.copy(todo = reasoner.todo
      .enqueue(Link(c, role, filler)))
    case _ => reasoner
  }

  // get from links indexed by target
  private def `R+∃`(ci: ConceptInclusion, reasoner: Reasoner): Reasoner = reasoner.copy(todo = reasoner.todo.enqueue(
    for {
      Link(e, r, ci.subclass) <- reasoner.links
      s <- reasoner.hier.getOrElse(r, Set.empty)
      f = ExistentialRestriction(s, ci.superclass)
      if reasoner.negExists(f)
    } yield ConceptInclusion(e, f)))

  // get from concIncs indexed by superclass
  private def `R⊑`(ci: ConceptInclusion, reasoner: Reasoner): Reasoner = reasoner.copy(todo = reasoner.todo.enqueue(
    reasoner.concIncs.collect { case ConceptInclusion(ci.superclass, e) => ConceptInclusion(ci.subclass, e) }))

  private def `R⊥`(link: Link, reasoner: Reasoner): Reasoner = {
    if (reasoner.subs(ConceptInclusion(link.target, Bottom)))
      reasoner.copy(todo = reasoner.todo.enqueue(ConceptInclusion(link.subject, Bottom)))
    else reasoner
  }

  // get from subs indexed by subclass
  private def `R+∃`(link: Link, reasoner: Reasoner): Reasoner = reasoner.copy(todo = reasoner.todo.enqueue(
    for {
      ConceptInclusion(link.target, superclass) <- reasoner.subs
      s <- reasoner.hier.getOrElse(link.role, Set.empty)
      f = ExistentialRestriction(s, superclass)
      if reasoner.negExists(f)
    } yield ConceptInclusion(link.subject, f)))

  // get from links indexed by subject
  private def `R∘`(link: Link, reasoner: Reasoner): Reasoner = reasoner.copy(todo = reasoner.todo.enqueue(
    for {
      Link(link.target, r2, d) <- reasoner.links
      s1 <- reasoner.hier.getOrElse(link.role, Set.empty)
      s2 <- reasoner.hier.getOrElse(r2, Set.empty)
      s <- reasoner.roleComps.getOrElse((s1, s2), Set.empty)
    } yield Link(link.subject, s, d)))

  private def `R⤳`(link: Link, reasoner: Reasoner): Reasoner = reasoner.copy(todo = reasoner.todo.enqueue(link.target))

}
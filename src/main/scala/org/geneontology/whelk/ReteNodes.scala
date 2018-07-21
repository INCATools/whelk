package org.geneontology.whelk

sealed trait AlphaNode[T] {

  def children: List[JoinNode[T]]

  def activate(item: T, reasoner: ReasonerState): ReasonerState

}

final case class ConceptAtomAlphaNode(children: List[JoinNode[Individual]], concept: Concept) extends AlphaNode[Individual] {

  def activate(individual: Individual, reasoner: ReasonerState): ReasonerState = {
    val wm = reasoner.wm
    val alphaMem = wm.conceptAlpha(concept)
    val updatedIndividuals = alphaMem.individuals + individual
    val updatedAlphaMem = alphaMem.copy(individuals = updatedIndividuals)
    val updatedConceptAlpha = wm.conceptAlpha.updated(concept, updatedAlphaMem)
    val updatedWM = wm.copy(conceptAlpha = updatedConceptAlpha)
    children.foldLeft(reasoner.copy(wm = updatedWM))((currentReasoner, child) => child.rightActivate(individual, currentReasoner))
  }

}

final case class RoleAtomAlphaNode(children: List[JoinNode[RoleAssertion]], role: Role) extends AlphaNode[RoleAssertion] {

  def activate(assertion: RoleAssertion, reasoner: ReasonerState): ReasonerState = {
    val wm = reasoner.wm
    val alphaMem = wm.roleAlpha(role)
    val updatedAssertions = assertion :: alphaMem.assertions
    val currentAssertionsBySubject = alphaMem.assertionsBySubject
    val updatedAssertionsBySubject = currentAssertionsBySubject.updated(
      assertion.subject,
      assertion :: currentAssertionsBySubject.getOrElse(assertion.subject, Nil))
    val currentAssertionsByTarget = alphaMem.assertionsByTarget
    val updatedAssertionsByTarget = currentAssertionsByTarget.updated(
      assertion.target,
      assertion :: currentAssertionsByTarget.getOrElse(assertion.target, Nil))
    val updatedAlphaMem = alphaMem.copy(assertions = updatedAssertions, assertionsBySubject = updatedAssertionsBySubject, assertionsByTarget = updatedAssertionsByTarget)
    val updatedRoleAlpha = wm.roleAlpha.updated(role, updatedAlphaMem)
    val updatedWM = wm.copy(roleAlpha = updatedRoleAlpha)
    children.foldLeft(reasoner.copy(wm = updatedWM))((currentReasoner, child) => child.rightActivate(assertion, currentReasoner))
  }

}

final case class JoinNodeSpec(pattern: List[RuleAtom]) {

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

object JoinNodeSpec {

  val empty: JoinNodeSpec = JoinNodeSpec(Nil)

}

sealed trait BetaNode {

  def spec: JoinNodeSpec

  def leftActivate(token: Token, reasoner: ReasonerState): ReasonerState

}

sealed trait BetaParent {

  def children: List[BetaNode]

}

object BetaRoot extends BetaNode with BetaParent {

  def leftActivate(token: Token, reasoner: ReasonerState): ReasonerState = reasoner
  val spec: JoinNodeSpec = JoinNodeSpec(Nil)
  val memory: BetaMemory = BetaMemory(Nil, Map.empty)
  val children = Nil

}

final case class Token(bindings: Map[Variable, Individual]) {

  def extend(newBindings: Map[Variable, Individual]): Token = this.copy(bindings = bindings ++ newBindings)

}

sealed trait JoinNode[T] extends BetaNode with BetaParent {

  val thisPattern = spec.pattern.head
  val parentBoundVariables = spec.pattern.drop(1).flatMap(_.variables).toSet
  val thisPatternVariables = thisPattern.variables
  val matchVariables = parentBoundVariables.intersect(thisPatternVariables)

  def rightActivate(item: T, reasoner: ReasonerState): ReasonerState

  protected[this] def activateChildren(newTokens: List[Token], reasoner: ReasonerState): ReasonerState = {
    newTokens.foldLeft(reasoner) { (currentReasoner, token) =>
      val localBetaMem = currentReasoner.wm.beta(spec)
      val updatedBetaTokens = token :: localBetaMem.tokens
      val updatedTokenIndex = token.bindings.foldLeft(localBetaMem.tokenIndex) {
        case (currentIndex, (variable, ind)) =>
          val bindings = currentIndex.getOrElse(variable, Map.empty)
          val updatedTokens = token :: bindings.getOrElse(ind, Nil)
          currentIndex.updated(variable, bindings.updated(ind, updatedTokens))
      }
      val updatedBetaMem = localBetaMem.copy(tokens = updatedBetaTokens, tokenIndex = updatedTokenIndex)
      val updatedBeta = currentReasoner.wm.beta.updated(spec, updatedBetaMem)
      val updatedWM = currentReasoner.wm.copy(beta = updatedBeta)
      children.foldLeft(currentReasoner.copy(wm = updatedWM)) { (moreCurrentReasoner, child) =>
        child.leftActivate(token, moreCurrentReasoner)
      }
    }
  }

}

final case class ConceptAtomJoinNode(atom: ConceptAtom, children: List[BetaNode], leftParent: BetaNode with BetaParent, rightParent: AlphaNode[Individual], spec: JoinNodeSpec) extends JoinNode[Individual] {

  def leftActivate(token: Token, reasoner: ReasonerState): ReasonerState = {
    val alphaMem = reasoner.wm.conceptAlpha(atom.predicate)
    val newTokens = atom.argument match {
      case variable: Variable if parentBoundVariables(variable) =>
        val ind = token.bindings(variable)
        if (alphaMem.individuals(ind)) List(token) else Nil
      case variable: Variable        => alphaMem.individuals.toList.map(i => token.extend(Map(variable -> i)))
      case individualArg: Individual => if (alphaMem.individuals(individualArg)) List(token) else Nil
    }
    activateChildren(newTokens, reasoner)
  }

  def rightActivate(individual: Individual, reasoner: ReasonerState): ReasonerState = {
    val parentMem = reasoner.wm.beta(leftParent.spec)
    val newTokens = atom.argument match {
      case variable: Variable => parentMem.tokenIndex.get(variable) match {
        case Some(bound) => bound.get(individual).getOrElse(Nil)
        case None        => parentMem.tokens.map(_.extend(Map(variable -> individual)))
      }
      case individualArg: Individual if (individualArg != individual) => Nil
      case individualArg: Individual                                  => parentMem.tokens
    }
    activateChildren(newTokens, reasoner)
  }

}

final case class RoleAtomJoinNode(atom: RoleAtom, children: List[BetaNode], leftParent: BetaNode with BetaParent, rightParent: AlphaNode[RoleAssertion], spec: JoinNodeSpec) extends JoinNode[RoleAssertion] {

  private val subjectMustBeSameAsTarget = atom.subject == atom.target

  private val makeBindings: RoleAssertion => Map[Variable, Individual] = atom match {
    case RoleAtom(_, subjectVar: Variable, targetVar: Variable) => (ra: RoleAssertion) => Map(subjectVar -> ra.subject, targetVar -> ra.target)
    case RoleAtom(_, _, targetVar: Variable) => (ra: RoleAssertion) => Map(targetVar -> ra.target)
    case RoleAtom(_, subjectVar: Variable, _) => (ra: RoleAssertion) => Map(subjectVar -> ra.subject)
    case _ => (ra: RoleAssertion) => Map.empty
  }

  def leftActivate(token: Token, reasoner: ReasonerState): ReasonerState = {
    val alphaMem = reasoner.wm.roleAlpha(atom.predicate)
    val goodSubjectAssertions = atom.subject match {
      case variable: Variable if parentBoundVariables(variable) =>
        val ind = token.bindings(variable)
        alphaMem.assertionsBySubject.getOrElse(ind, Nil)
      case variable: Variable        => alphaMem.assertions
      case individualArg: Individual => alphaMem.assertionsBySubject.getOrElse(individualArg, Nil)
    }
    val goodTargetAssertions = atom.target match {
      case variable: Variable if parentBoundVariables(variable) =>
        val ind = token.bindings(variable)
        alphaMem.assertionsByTarget.getOrElse(ind, Nil)
      case variable: Variable        => alphaMem.assertions
      case individualArg: Individual => alphaMem.assertionsByTarget.getOrElse(individualArg, Nil)
    }
    val newAssertions = goodSubjectAssertions.intersect(goodTargetAssertions)
      .filterNot(ra => (subjectMustBeSameAsTarget && (ra.subject != ra.target)))
    val newTokens = newAssertions.map(ra => token.extend(makeBindings(ra)))
    activateChildren(newTokens, reasoner)
  }

  def rightActivate(assertion: RoleAssertion, reasoner: ReasonerState): ReasonerState = {
    if (subjectMustBeSameAsTarget && (assertion.subject != assertion.target)) reasoner
    else {
      val parentMem = reasoner.wm.beta(leftParent.spec)
      val (goodSubjectTokens, newSubjectBindings: Map[Variable, Individual]) = atom.subject match {
        case variable: Variable => parentMem.tokenIndex.get(variable) match {
          case Some(bound) => (bound.get(assertion.subject).getOrElse(Nil), Map.empty)
          case None        => (parentMem.tokens, Map(variable -> assertion.subject))
        }
        case individualArg: Individual if (individualArg != assertion.subject) => (Nil, Map.empty)
        case individualArg: Individual                                         => (parentMem.tokens, Map.empty)
      }
      val (goodTargetTokens, newTargetBindings: Map[Variable, Individual]) = if (goodSubjectTokens.nonEmpty) atom.target match {
        case variable: Variable => parentMem.tokenIndex.get(variable) match {
          case Some(bound) => (bound.get(assertion.target).getOrElse(Nil), Map.empty)
          case None        => (parentMem.tokens, Map(variable -> assertion.target))
        }
        case individualArg: Individual if (individualArg != assertion.target) => (Nil, Map.empty)
        case individualArg: Individual                                        => (parentMem.tokens, Map.empty)
      }
      else (Nil, Map.empty)
      val newTokens = goodSubjectTokens.intersect(goodTargetTokens).map(t => t.extend(newSubjectBindings).extend(newTargetBindings))
      activateChildren(newTokens, reasoner)
    }
  }

}

final case class ProductionNode(rule: Rule, parent: BetaNode) extends BetaNode {

  def leftActivate(token: Token, reasoner: ReasonerState): ReasonerState = {
    var todo = reasoner.todo
    for {
      atom <- rule.head
    } {
      atom match {
        case RoleAtom(role, subj, obj) => todo = Link(Nominal(fillVariable(subj, token)), role, Nominal(fillVariable(obj, token))) :: todo
        case ConceptAtom(concept, arg) => todo = ConceptInclusion(Nominal(fillVariable(arg, token)), concept) :: todo
      }
    }
    reasoner.copy(todo = todo)
  }

  private def fillVariable(arg: IndividualArgument, token: Token): Individual = arg match {
    case v: Variable   => token.bindings(v)
    case i: Individual => i
  }

  val spec = JoinNodeSpec.empty

}
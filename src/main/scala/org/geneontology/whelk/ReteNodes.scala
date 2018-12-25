package org.geneontology.whelk

sealed trait AlphaNode[T] {

  def children: List[JoinNode[T]]

  def activate(item: T, reasoner: ReasonerState): ReasonerState

}

final case class ConceptAtomAlphaNode(concept: Concept, children: List[JoinNode[Individual]]) extends AlphaNode[Individual] {

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

final case class RoleAtomAlphaNode(role: Role, children: List[JoinNode[RoleAssertion]]) extends AlphaNode[RoleAssertion] {

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

  def leftActivate(token: Token, reasoner: ReasonerState): ReasonerState

}

sealed trait BetaParent {

  def children: List[BetaNode]

}

final case class Token(bindings: Map[Variable, Individual]) {

  def extend(newBindings: Map[Variable, Individual]): Token = this.copy(bindings = bindings ++ newBindings)

}

object Token {

  val empty: Token = Token(Map.empty)

}

sealed trait JoinNode[T] extends BetaNode with BetaParent {

  def spec: JoinNodeSpec

  protected[this] val thisPattern: RuleAtom = spec.pattern.head
  protected[this] val leftParentSpec: JoinNodeSpec = JoinNodeSpec(spec.pattern.drop(1))
  protected[this] val parentBoundVariables: Set[Variable] = leftParentSpec.pattern.flatMap(_.variables).toSet
  protected[this] val thisPatternVariables: Set[Variable] = thisPattern.variables
  protected[this] val matchVariables: Set[Variable] = parentBoundVariables.intersect(thisPatternVariables)

  def rightActivate(item: T, reasoner: ReasonerState): ReasonerState

  protected[this] def activateChildren(newTokens: List[Token], reasoner: ReasonerState): ReasonerState = {
    newTokens.foldLeft(reasoner) { (currentReasoner, token) =>
      val localBetaMem = currentReasoner.wm.beta(spec)
      val updatedBetaTokens = token :: localBetaMem.tokens
      val updatedTokenIndex = token.bindings.foldLeft(localBetaMem.tokenIndex) {
        case (currentIndex, (variable, ind)) =>
          val bindings = currentIndex.getOrElse(variable, Map.empty)
          val updatedTokens = bindings.getOrElse(ind, Set.empty) + token
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

final case class ConceptAtomJoinNode(atom: ConceptAtom, children: List[BetaNode], spec: JoinNodeSpec) extends JoinNode[Individual] {

  def leftActivate(token: Token, reasoner: ReasonerState): ReasonerState = {
    val alphaMem = reasoner.wm.conceptAlpha(atom.predicate)
    val newTokens = atom.argument match {
      case variable: Variable if parentBoundVariables(variable) =>
        val ind = token.bindings(variable)
        if (alphaMem.individuals(ind)) List(token) else Nil
      case variable: Variable                                   => alphaMem.individuals.toList.map(i => token.extend(Map(variable -> i)))
      case individualArg: Individual                            => if (alphaMem.individuals(individualArg)) List(token) else Nil
    }
    activateChildren(newTokens, reasoner)
  }

  def rightActivate(individual: Individual, reasoner: ReasonerState): ReasonerState = {
    val parentMem = reasoner.wm.beta(leftParentSpec)
    val newTokens = atom.argument match {
      case variable: Variable                                       => parentMem.tokenIndex.get(variable) match {
        case Some(bound) => bound.getOrElse(individual, Nil).toList
        case None        => parentMem.tokens.map(_.extend(Map(variable -> individual)))
      }
      case individualArg: Individual if individualArg != individual => Nil
      case _: Individual                                            => parentMem.tokens
    }
    activateChildren(newTokens, reasoner)
  }

}

final case class RoleAtomJoinNode(atom: RoleAtom, children: List[BetaNode], spec: JoinNodeSpec) extends JoinNode[RoleAssertion] {

  private val subjectMustBeSameAsTarget = atom.subject == atom.target

  private val makeBindings: RoleAssertion => Map[Variable, Individual] = atom match {
    case RoleAtom(_, subjectVar: Variable, targetVar: Variable) => (ra: RoleAssertion) => Map(subjectVar -> ra.subject, targetVar -> ra.target)
    case RoleAtom(_, _, targetVar: Variable)                    => (ra: RoleAssertion) => Map(targetVar -> ra.target)
    case RoleAtom(_, subjectVar: Variable, _)                   => (ra: RoleAssertion) => Map(subjectVar -> ra.subject)
    case _                                                      => (_: RoleAssertion) => Map.empty
  }

  val findTokensForAssertion: (RoleAssertion, BetaMemory) => List[Token] = (atom.subject, atom.target) match {

    case (subjectVariable: Variable, targetVariable: Variable) if parentBoundVariables(subjectVariable) && parentBoundVariables(targetVariable) =>
      (assertion: RoleAssertion, parentMem: BetaMemory) => {
        val subjectTokens = parentMem.tokenIndex(subjectVariable).getOrElse(assertion.subject, Set.empty)
        if (subjectTokens.nonEmpty)
          subjectTokens.intersect(parentMem.tokenIndex(targetVariable).getOrElse(assertion.target, Set.empty)).toList
        else Nil
      }
    case (subjectVariable: Variable, _: Variable) if parentBoundVariables(subjectVariable)                                                      =>
      (assertion: RoleAssertion, parentMem: BetaMemory) => parentMem.tokenIndex(subjectVariable).getOrElse(assertion.subject, Set.empty).toList.map(t => t.extend(makeBindings(assertion)))

    case (_: Variable, targetVariable: Variable) if parentBoundVariables(targetVariable) =>
      (assertion: RoleAssertion, parentMem: BetaMemory) => parentMem.tokenIndex(targetVariable).getOrElse(assertion.target, Set.empty).toList.map(t => t.extend(makeBindings(assertion)))

    case (subjectVariable: Variable, individualArg: Individual) if parentBoundVariables(subjectVariable) =>
      (assertion: RoleAssertion, parentMem: BetaMemory) => {
        if (individualArg == assertion.target)
          parentMem.tokenIndex(subjectVariable).getOrElse(assertion.subject, Set.empty).toList.map(t => t.extend(makeBindings(assertion)))
        else Nil
      }
    case (individualArg: Individual, targetVariable: Variable) if parentBoundVariables(targetVariable)   =>
      (assertion: RoleAssertion, parentMem: BetaMemory) => {
        if (individualArg == assertion.subject)
          parentMem.tokenIndex(targetVariable).getOrElse(assertion.target, Set.empty).toList.map(t => t.extend(makeBindings(assertion)))
        else Nil
      }
    case (individualSubject: Individual, individualTarget: Individual)                                   =>
      (assertion: RoleAssertion, parentMem: BetaMemory) => {
        if ((individualSubject == assertion.subject) && (individualTarget == assertion.target)) parentMem.tokens
        else Nil
      }
    //FIXME missing ind with unbound var? both directions
    case (_, _) => (assertion: RoleAssertion, parentMem: BetaMemory) => parentMem.tokens.map(t => t.extend(makeBindings(assertion)))

  }

  def findAssertionsForToken: (Token, RoleAlphaMemory) => List[RoleAssertion] = {
    (atom.subject, atom.target) match {

      case (subjectVariable: Variable, targetVariable: Variable) if parentBoundVariables(subjectVariable) && parentBoundVariables(targetVariable) =>
        (token: Token, alphaMem: RoleAlphaMemory) => {
          val subjectAssertions = alphaMem.assertionsBySubject.getOrElse(token.bindings(subjectVariable), Nil)
          if (subjectAssertions.nonEmpty)
            subjectAssertions.intersect(alphaMem.assertionsByTarget.getOrElse(token.bindings(targetVariable), Nil))
          else Nil
        }
      case (subjectVariable: Variable, _: Variable) if parentBoundVariables(subjectVariable)                                                      =>
        (token: Token, alphaMem: RoleAlphaMemory) =>
          alphaMem.assertionsBySubject.getOrElse(token.bindings(subjectVariable), Nil)

      case (_: Variable, targetVariable: Variable) if parentBoundVariables(targetVariable) =>
        (token: Token, alphaMem: RoleAlphaMemory) =>
          alphaMem.assertionsByTarget.getOrElse(token.bindings(targetVariable), Nil)

      case (subjectVariable: Variable, individualArg: Individual) if parentBoundVariables(subjectVariable) =>
        (token: Token, alphaMem: RoleAlphaMemory) => {
          val subjectAssertions = alphaMem.assertionsBySubject.getOrElse(token.bindings(subjectVariable), Nil)
          if (subjectAssertions.nonEmpty)
            subjectAssertions.intersect(alphaMem.assertionsByTarget.getOrElse(individualArg, Nil))
          else Nil
        }

      case (individualArg: Individual, targetVariable: Variable) if parentBoundVariables(targetVariable) =>
        (token: Token, alphaMem: RoleAlphaMemory) => {
          val subjectAssertions = alphaMem.assertionsBySubject.getOrElse(individualArg, Nil)
          if (subjectAssertions.nonEmpty)
            subjectAssertions.intersect(alphaMem.assertionsByTarget.getOrElse(token.bindings(targetVariable), Nil))
          else Nil
        }

      case (_: Variable, individualArg: Individual) =>
        (_: Token, alphaMem: RoleAlphaMemory) =>
          alphaMem.assertionsByTarget.getOrElse(individualArg, Nil)

      case (individualArg: Individual, _: Variable) =>
        (_: Token, alphaMem: RoleAlphaMemory) =>
          alphaMem.assertionsBySubject.getOrElse(individualArg, Nil)

      case (individualSubject: Individual, individualTarget: Individual) =>
        (_: Token, alphaMem: RoleAlphaMemory) =>
          alphaMem.assertionsBySubject.getOrElse(individualSubject, Nil).filter(_.target == individualTarget)

      case (_, _) => (_: Token, alphaMem: RoleAlphaMemory) => alphaMem.assertions

    }
  }

  def leftActivate(token: Token, reasoner: ReasonerState): ReasonerState = {
    val alphaMem = reasoner.wm.roleAlpha(atom.predicate)
    val potentialAssertions = findAssertionsForToken(token, alphaMem)
    val goodAssertions = if (subjectMustBeSameAsTarget) potentialAssertions.filter(ra => ra.subject == ra.target)
    else potentialAssertions
    val newTokens = goodAssertions.map(ra => token.extend(makeBindings(ra)))
    activateChildren(newTokens, reasoner)
  }

  def rightActivate(assertion: RoleAssertion, reasoner: ReasonerState): ReasonerState = {
    if (subjectMustBeSameAsTarget && (assertion.subject != assertion.target)) reasoner
    else {
      val parentMem = reasoner.wm.beta(leftParentSpec)
      val newTokens = findTokensForAssertion(assertion, parentMem)
      activateChildren(newTokens, reasoner)
    }
  }

}

final case class ProductionNode(rule: Rule) extends BetaNode {

  def leftActivate(token: Token, reasoner: ReasonerState): ReasonerState =
    rule.head.foldLeft(reasoner) { (state, atom) =>
      atom match {
        case RoleAtom(role, subj, obj) => Reasoner.processLink(Nominal(fillVariable(subj, token)), role, Nominal(fillVariable(obj, token)), state)
        case ConceptAtom(concept, arg) => Reasoner.processConceptInclusion(Nominal(fillVariable(arg, token)), concept, state)
      }
    }

  private def fillVariable(arg: IndividualArgument, token: Token): Individual = arg match {
    case v: Variable   => token.bindings(v)
    case i: Individual => i
  }

}
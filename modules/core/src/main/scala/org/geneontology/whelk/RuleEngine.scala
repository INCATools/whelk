package org.geneontology.whelk

import scala.collection.mutable

final case class RuleEngine(rules: Set[Rule]) {

  val (conceptAlphaIndex: Map[Concept, ConceptAtomAlphaNode],
  roleAlphaIndex: Map[Role, RoleAtomAlphaNode],
  allJoinSpecs: Set[JoinNodeSpec]) = constructReteNetwork(rules)

  def emptyMemory: WorkingMemory = WorkingMemory(
    conceptAlphaIndex.keys.map(c => c -> ConceptAlphaMemory.empty).toMap,
    roleAlphaIndex.keys.map(r => r -> RoleAlphaMemory.empty).toMap,
    //making entries for all variables; can any getOrElse be removed?
    allJoinSpecs.map(s => s -> BetaMemory(Nil, s.pattern.flatMap(_.variables).map(_ -> Map.empty[Individual, Set[Token]]).toMap)).toMap + (JoinNodeSpec.empty -> BetaMemory(List(Token.empty), Map.empty)))

  def processConceptAssertion(assertion: ConceptAssertion, reasoner: ReasonerState, todo: mutable.Stack[QueueExpression]): ReasonerState =
    conceptAlphaIndex.get(assertion.concept).map(node => node.activate(assertion.individual, reasoner, todo)).getOrElse(reasoner)

  def processRoleAssertion(assertion: RoleAssertion, reasoner: ReasonerState, todo: mutable.Stack[QueueExpression]): ReasonerState =
    roleAlphaIndex.get(assertion.role).map(node => node.activate(assertion, reasoner, todo)).getOrElse(reasoner)

  private def constructReteNetwork(rules: Set[Rule]): (Map[Concept, ConceptAtomAlphaNode], Map[Role, RoleAtomAlphaNode], Set[JoinNodeSpec]) = {
    val nodes = rules.foldLeft(Set.empty[BetaNode]) { (nodes, rule) =>
      val (_, allConceptNodes, allRoleNodes) = processRuleAtoms(rule.body, Nil, rule, Map.empty, Map.empty)
      nodes ++ allConceptNodes.values ++ allRoleNodes.values
    }
    val joinNodes = nodes.collect { case j: JoinNode[_] => j }
    val allSpecs = joinNodes.map(_.spec)
    val ancestorMap = joinNodes.foldLeft(Map.empty[BetaNode, Set[BetaNode]]) { (ancestors, node) =>
      node.children.foldLeft(ancestors) { (updatedAncestors, child) =>
        updatedAncestors.updated(child, updatedAncestors.getOrElse(child, Set.empty) + node)
      }
    }
    val conceptAlphas = nodes.collect { case n: ConceptAtomJoinNode => n }.groupBy(_.atom.predicate).map {
      case (concept, childJoinNodes) => concept -> ConceptAtomAlphaNode(concept, orderChildren(childJoinNodes.toList, ancestorMap))
    }
    val roleAlphas = nodes.collect { case n: RoleAtomJoinNode => n }.groupBy(_.atom.predicate).map {
      case (role, childJoinNodes) => role -> RoleAtomAlphaNode(role, orderChildren(childJoinNodes.toList, ancestorMap))
    }
    (conceptAlphas, roleAlphas, allSpecs)
  }

  private def processRuleAtoms(atoms: List[RuleAtom], parentAtoms: List[RuleAtom], rule: Rule, existingC: Map[JoinNodeSpec, ConceptAtomJoinNode], existingR: Map[JoinNodeSpec, RoleAtomJoinNode]): (BetaNode, Map[JoinNodeSpec, ConceptAtomJoinNode], Map[JoinNodeSpec, RoleAtomJoinNode]) =
    atoms match {
      case atom :: rest =>
        val thisPatternSequence = atom :: parentAtoms
        val spec = JoinNodeSpec(thisPatternSequence)
        val (child, updatedExistingC, updatedExistingR) = processRuleAtoms(rest, thisPatternSequence, rule, existingC, existingR)
        atom match {
          case ca: ConceptAtom =>
            val node = existingC.getOrElse(spec, ConceptAtomJoinNode(ca, Nil, spec))
            val updatedNode = node.copy(children = child :: node.children)
            (updatedNode, updatedExistingC.updated(spec, updatedNode), updatedExistingR)
          case ra: RoleAtom    =>
            val node = existingR.getOrElse(spec, RoleAtomJoinNode(ra, Nil, spec))
            val updatedNode = node.copy(children = child :: node.children)
            (updatedNode, updatedExistingC, updatedExistingR.updated(spec, updatedNode))
        }
      case Nil          => (ProductionNode(rule), existingC, existingR)
    }

  private def orderChildren[T](children: List[JoinNode[T]], ancestorMap: Map[BetaNode, Set[BetaNode]]): List[JoinNode[T]] =
    children.sortWith((a, b) => !ancestorMap.getOrElse(b, Set.empty)(a))

}

object RuleEngine {

  val empty: RuleEngine = RuleEngine(Set.empty)

}
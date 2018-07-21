package org.geneontology.whelk

final case class WorkingMemory(conceptAlpha: Map[Concept, ConceptAlphaMemory], roleAlpha: Map[Role, RoleAlphaMemory], beta: Map[JoinNodeSpec, BetaMemory])

object WorkingMemory {

  val empty: WorkingMemory = WorkingMemory(Map.empty, Map.empty, Map.empty)

}

final case class ConceptAlphaMemory(individuals: Set[Individual])

final case class RoleAlphaMemory(assertions: List[RoleAssertion], assertionsBySubject: Map[Individual, List[RoleAssertion]], assertionsByTarget: Map[Individual, List[RoleAssertion]])

final case class BetaMemory(tokens: List[Token], tokenIndex: Map[Variable, Map[Individual, List[Token]]])

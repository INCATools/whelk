package org.geneontology.whelk

final case class WorkingMemory(conceptAlpha: Map[Concept, ConceptAlphaMemory], roleAlpha: Map[Role, RoleAlphaMemory], beta: Map[JoinNodeSpec, BetaMemory])

final case class ConceptAlphaMemory(individuals: Set[Individual])

object ConceptAlphaMemory {

  val empty: ConceptAlphaMemory = ConceptAlphaMemory(Set.empty)

}

final case class RoleAlphaMemory(assertions: List[RoleAssertion], assertionsBySubject: Map[Individual, List[RoleAssertion]], assertionsByTarget: Map[Individual, List[RoleAssertion]])

object RoleAlphaMemory {

  val empty: RoleAlphaMemory = RoleAlphaMemory(Nil, Map.empty, Map.empty)

}

final case class BetaMemory(tokens: List[Token], tokenIndex: Map[Variable, Map[Individual, List[Token]]])

object BetaMemory {

  val empty: BetaMemory = BetaMemory(Nil, Map.empty)

}

package org.geneontology.whelk

sealed trait QueueExpression

final case class Role(id: String)

sealed trait Concept extends QueueExpression {

  def conceptSignature: Set[Concept]

}

final case class AtomicConcept(id: String) extends Concept {

  def conceptSignature: Set[Concept] = Set(this)

  override val hashCode = id.hashCode

}

final case object Top extends Concept {

  def conceptSignature: Set[Concept] = Set(this)

  override val hashCode: Int = super.hashCode

}

final case object Bottom extends Concept {

  def conceptSignature: Set[Concept] = Set(this)

  override val hashCode: Int = super.hashCode

}

final case class Conjunction(left: Concept, right: Concept) extends Concept {

  def conceptSignature: Set[Concept] = left.conceptSignature ++ right.conceptSignature + this

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class ExistentialRestriction(role: Role, concept: Concept) extends Concept {

  def conceptSignature: Set[Concept] = concept.conceptSignature + this

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class Individual(id: String)

sealed trait Axiom

final case class ConceptInclusion(subclass: Concept, superclass: Concept) extends Axiom with QueueExpression {

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class RoleInclusion(subproperty: Role, superproperty: Role) extends Axiom

final case class RoleComposition(first: Role, second: Role, superproperty: Role) extends Axiom

final case class ConceptAssertion(concept: Concept, individual: Individual) extends Axiom

final case class RoleAssertion(role: Role, subject: Individual, target: Individual) extends Axiom

final case class Link(subject: Concept, role: Role, target: Concept) extends QueueExpression {

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}


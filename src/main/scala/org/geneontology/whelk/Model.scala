package org.geneontology.whelk

sealed trait QueueExpression

sealed trait Entity

final case class Role(id: String) extends Entity {

  override val hashCode = scala.util.hashing.MurmurHash3.productHash(this)

}

sealed trait Concept extends QueueExpression {

  def conceptSignature: Set[Concept]

  def signature: Set[Entity]

}

final case class AtomicConcept(id: String) extends Concept with Entity {

  def conceptSignature: Set[Concept] = Set(this)

  def signature: Set[Entity] = Set(this)

  override val hashCode = scala.util.hashing.MurmurHash3.productHash(this)

}

final object BuiltIn {

  private val owl = "http://www.w3.org/2002/07/owl"

  final val Top = AtomicConcept(s"$owl#Thing")

  final val Bottom = AtomicConcept(s"$owl#Nothing")

}

final case class Conjunction(left: Concept, right: Concept) extends Concept {

  def conceptSignature: Set[Concept] = left.conceptSignature ++ right.conceptSignature + this

  def signature: Set[Entity] = left.signature ++ right.signature

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class ExistentialRestriction(role: Role, concept: Concept) extends Concept {

  def conceptSignature: Set[Concept] = concept.conceptSignature + this

  def signature: Set[Entity] = concept.signature + role

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class Individual(id: String) extends Entity

sealed trait Axiom {

  def signature: Set[Entity]

}

final case class ConceptInclusion(subclass: Concept, superclass: Concept) extends Axiom with QueueExpression {

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

  def signature: Set[Entity] = subclass.signature ++ superclass.signature

}

final case class RoleInclusion(subproperty: Role, superproperty: Role) extends Axiom {

  def signature: Set[Entity] = Set(subproperty, superproperty)

}

final case class RoleComposition(first: Role, second: Role, superproperty: Role) extends Axiom {

  def signature: Set[Entity] = Set(first, second, superproperty)

}

final case class ConceptAssertion(concept: Concept, individual: Individual) extends Axiom {

  def signature: Set[Entity] = concept.signature + individual

}

final case class RoleAssertion(role: Role, subject: Individual, target: Individual) extends Axiom {

  def signature: Set[Entity] = Set(role, subject, target)

}

final case class Link(subject: Concept, role: Role, target: Concept) extends QueueExpression {

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class `Sub+`(ci: ConceptInclusion) extends QueueExpression

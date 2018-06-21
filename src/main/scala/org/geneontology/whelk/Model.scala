package org.geneontology.whelk

import upickle.default._

sealed trait QueueExpression

object QueueExpression {

  implicit def rw: ReadWriter[QueueExpression] = macroRW

}

sealed trait Entity

final case class Role(id: String) extends Entity {

  override val hashCode = scala.util.hashing.MurmurHash3.productHash(this)

}

object Role {

  implicit def rw: ReadWriter[Role] = macroRW

}

sealed trait Concept extends QueueExpression {

  def conceptSignature: Set[Concept]

  def signature: Set[Entity]

}

object Concept {

  implicit def rw: ReadWriter[Concept] = ReadWriter.merge(AtomicConcept.rw, Conjunction.rw, ExistentialRestriction.rw)

}

final case class AtomicConcept(id: String) extends Concept with Entity {

  def conceptSignature: Set[Concept] = Set(this)

  def signature: Set[Entity] = Set(this)

  override val hashCode = scala.util.hashing.MurmurHash3.productHash(this)

}

object AtomicConcept {

  implicit def rw: ReadWriter[AtomicConcept] = macroRW

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

object Conjunction {

  implicit def rw: ReadWriter[Conjunction] = macroRW

}

final case class ExistentialRestriction(role: Role, concept: Concept) extends Concept {

  def conceptSignature: Set[Concept] = concept.conceptSignature + this

  def signature: Set[Entity] = concept.signature + role

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

object ExistentialRestriction {

  implicit def rw: ReadWriter[ExistentialRestriction] = macroRW

}

final case class Individual(id: String) extends Entity

sealed trait Axiom {

  def signature: Set[Entity]

}

final case class ConceptInclusion(subclass: Concept, superclass: Concept) extends Axiom with QueueExpression {

  def signature: Set[Entity] = subclass.signature ++ superclass.signature

}

object ConceptInclusion {

  implicit def rw: ReadWriter[ConceptInclusion] = macroRW

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

final case class Link(subject: Concept, role: Role, target: Concept) extends QueueExpression

object Link {

  implicit def rw: ReadWriter[Link] = macroRW

}

final case class `Sub+`(ci: ConceptInclusion) extends QueueExpression

object `Sub+` {

  implicit def rw: ReadWriter[`Sub+`] = macroRW

}

package org.geneontology.whelk

sealed trait QueueExpression

sealed trait Entity

trait HasSignature {

  def signature: Set[Entity]

}

final case class Role(id: String) extends Entity {

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

object Role {

  def apply(id: String): Role = new Role(id.intern())

}

final case class DataRole(id: String) extends Entity {

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

object DataRole {

  def apply(id: String): DataRole = new DataRole(id.intern())

}

sealed trait Concept extends QueueExpression with HasSignature {

  def conceptSignature: Set[Concept]

  def isAnonymous: Boolean

}

final case class AtomicConcept(id: String) extends Concept with Entity {

  def conceptSignature: Set[Concept] = Set(this)

  def signature: Set[Entity] = Set(this)

  def isAnonymous: Boolean = false

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

object AtomicConcept {

  def apply(id: String): AtomicConcept = new AtomicConcept(id.intern())

}

object BuiltIn {

  private val owl = "http://www.w3.org/2002/07/owl"

  final val Top = AtomicConcept(s"$owl#Thing")

  final val Bottom = AtomicConcept(s"$owl#Nothing")

}

final case class Conjunction(left: Concept, right: Concept) extends Concept {

  def conceptSignature: Set[Concept] = left.conceptSignature ++ right.conceptSignature + this

  def signature: Set[Entity] = left.signature ++ right.signature

  def isAnonymous: Boolean = true

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class Disjunction(operands: Set[Concept]) extends Concept {

  def conceptSignature: Set[Concept] = operands.flatMap(_.conceptSignature) + this

  def signature: Set[Entity] = operands.flatMap(_.signature)

  def isAnonymous: Boolean = true

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class ExistentialRestriction(role: Role, concept: Concept) extends Concept {

  def conceptSignature: Set[Concept] = concept.conceptSignature + this

  def signature: Set[Entity] = concept.signature + role

  def isAnonymous: Boolean = true

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class SelfRestriction(role: Role) extends Concept {

  def conceptSignature: Set[Concept] = Set(this)

  def signature: Set[Entity] = Set(role)

  def isAnonymous: Boolean = true

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}


//FIXME this is just a placeholder implementation for basic reasoning with identity
final case class DataRange(owl: AnyRef) {

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class DataRestriction(role: DataRole, range: DataRange) extends Concept {

  def conceptSignature: Set[Concept] = Set(this)

  def signature: Set[Entity] = Set(role)

  def isAnonymous: Boolean = true

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

//FIXME this is just a placeholder implementation for basic reasoning with identity
final case class DataHasValue(role: DataRole, value: AnyRef) extends Concept {

  def conceptSignature: Set[Concept] = Set(this)

  def signature: Set[Entity] = Set(role)

  def isAnonymous: Boolean = true

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class Complement(concept: Concept) extends Concept {

  def conceptSignature: Set[Concept] = concept.conceptSignature + this

  def signature: Set[Entity] = concept.signature

  def isAnonymous: Boolean = true

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class Individual(id: String) extends Entity with IndividualArgument {

  def signature: Set[Entity] = Set(this)

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

sealed trait Axiom extends HasSignature

final case class Nominal(individual: Individual) extends Concept {

  def conceptSignature: Set[Concept] = Set(this)

  def signature: Set[Entity] = Set(individual)

  def isAnonymous: Boolean = true

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class ConceptInclusion(subclass: Concept, superclass: Concept) extends Axiom with QueueExpression {

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

  override val hashCode: Int = scala.util.hashing.MurmurHash3.productHash(this)

}

final case class Link(subject: Concept, role: Role, target: Concept) extends QueueExpression

final case class `Sub+`(ci: ConceptInclusion) extends QueueExpression

sealed trait IndividualArgument extends HasSignature

final case class Variable(id: String) extends IndividualArgument {

  def signature: Set[Entity] = Set.empty

}

sealed trait RuleAtom extends HasSignature {

  def variables: Set[Variable]

}

final case class ConceptAtom(predicate: Concept, argument: IndividualArgument) extends RuleAtom {

  def signature: Set[Entity] = predicate.signature ++ argument.signature

  def variables: Set[Variable] = argument match {
    case v: Variable => Set(v)
    case _           => Set.empty
  }

}

final case class RoleAtom(predicate: Role, subject: IndividualArgument, target: IndividualArgument) extends RuleAtom {

  def signature: Set[Entity] = subject.signature ++ target.signature + predicate

  def variables: Set[Variable] = Set(subject, target).collect { case v: Variable => v }

}

final case class Rule(body: List[RuleAtom], head: List[RuleAtom]) extends Axiom {

  def signature: Set[Entity] = body.flatMap(_.signature).toSet ++ head.flatMap(_.signature).toSet

}

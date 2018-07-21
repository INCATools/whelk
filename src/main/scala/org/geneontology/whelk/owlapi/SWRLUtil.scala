package org.geneontology.whelk.owlapi

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.IRI
import org.semanticweb.owlapi.model.OWLClassExpression
import org.semanticweb.owlapi.model.OWLIndividual
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.SWRLClassAtom
import org.semanticweb.owlapi.model.SWRLDifferentIndividualsAtom
import org.semanticweb.owlapi.model.SWRLIArgument
import org.semanticweb.owlapi.model.SWRLIndividualArgument
import org.semanticweb.owlapi.model.SWRLObjectPropertyAtom
import org.semanticweb.owlapi.model.SWRLSameIndividualAtom
import org.semanticweb.owlapi.model.SWRLVariable

object SWRLUtil {

  private val factory = OWLManager.getOWLDataFactory

  object ClassAtom {

    def apply(predicate: OWLClassExpression, argument: SWRLIArgument): SWRLClassAtom =
      factory.getSWRLClassAtom(predicate, argument)

    def unapply(atom: SWRLClassAtom): Option[(OWLClassExpression, SWRLIArgument)] =
      Option((atom.getPredicate, atom.getArgument))

  }

  object ObjectPropertyAtom {

    def apply(predicate: OWLObjectPropertyExpression, subj: SWRLIArgument, obj: SWRLIArgument): SWRLObjectPropertyAtom =
      factory.getSWRLObjectPropertyAtom(predicate, subj, obj)

    def unapply(atom: SWRLObjectPropertyAtom): Option[(OWLObjectPropertyExpression, SWRLIArgument, SWRLIArgument)] =
      Option((atom.getPredicate, atom.getFirstArgument, atom.getSecondArgument))

  }

  object SameIndividualAtom {

    def apply(arg1: SWRLIArgument, arg2: SWRLIArgument): SWRLSameIndividualAtom =
      factory.getSWRLSameIndividualAtom(arg1, arg2)

    def unapply(atom: SWRLSameIndividualAtom): Option[(SWRLIArgument, SWRLIArgument)] =
      Option((atom.getFirstArgument, atom.getSecondArgument))

  }

  object DifferentIndividualsAtom {

    def apply(arg1: SWRLIArgument, arg2: SWRLIArgument): SWRLDifferentIndividualsAtom =
      factory.getSWRLDifferentIndividualsAtom(arg1, arg2)

    def unapply(atom: SWRLDifferentIndividualsAtom): Option[(SWRLIArgument, SWRLIArgument)] =
      Option((atom.getFirstArgument, atom.getSecondArgument))

  }

  object IndividualArg {

    def apply(individual: OWLIndividual): SWRLIndividualArgument = factory.getSWRLIndividualArgument(individual)

    def unapply(arg: SWRLIndividualArgument): Option[OWLIndividual] = Option(arg.getIndividual)

  }

  object Variable {

    def apply(iri: IRI): SWRLVariable = factory.getSWRLVariable(iri)

    def unapply(arg: SWRLVariable): Option[IRI] = Option(arg.getIRI)

  }

}
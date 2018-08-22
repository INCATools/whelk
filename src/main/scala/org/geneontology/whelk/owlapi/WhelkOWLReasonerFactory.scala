package org.geneontology.whelk.owlapi

import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.reasoner.BufferingMode
import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory

class WhelkOWLReasonerFactory extends OWLReasonerFactory {

  def createNonBufferingReasoner(ontology: OWLOntology): OWLReasoner = new WhelkOWLReasoner(ontology, BufferingMode.NON_BUFFERING)

  def createNonBufferingReasoner(ontology: OWLOntology, config: OWLReasonerConfiguration): OWLReasoner = new WhelkOWLReasoner(ontology, BufferingMode.NON_BUFFERING)

  def createReasoner(ontology: OWLOntology): OWLReasoner = new WhelkOWLReasoner(ontology, BufferingMode.BUFFERING)

  def createReasoner(ontology: OWLOntology, config: OWLReasonerConfiguration): OWLReasoner = new WhelkOWLReasoner(ontology, BufferingMode.BUFFERING)

  def getReasonerName: String = "Whelk"

}
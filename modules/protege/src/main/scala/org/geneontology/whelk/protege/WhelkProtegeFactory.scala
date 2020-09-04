package org.geneontology.whelk.protege

import org.geneontology.whelk.owlapi.WhelkOWLReasonerFactory
import org.protege.editor.owl.model.inference.AbstractProtegeOWLReasonerInfo
import org.semanticweb.owlapi.reasoner.{BufferingMode, OWLReasonerFactory}

class WhelkProtegeFactory extends AbstractProtegeOWLReasonerInfo {

  private val factory: OWLReasonerFactory = new WhelkOWLReasonerFactory()

  override def getRecommendedBuffering: BufferingMode = BufferingMode.BUFFERING

  override def getReasonerFactory: OWLReasonerFactory = factory

}

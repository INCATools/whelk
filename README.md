[![Build Status](https://travis-ci.org/balhoff/whelk.svg?branch=master)](https://travis-ci.org/balhoff/whelk)

<img align="right" src="https://farm7.staticflickr.com/6205/6045158767_e70d43139d_m_d.jpg">

# Whelk

Whelk is an OWL reasoner based on the algorithm implemented in [ELK](https://github.com/liveontologies/elk-reasoner), as described in [The Incredible ELK](https://doi.org/10.1007/s10817-013-9296-3).

Whelk is implemented as an immutable functional data structure, so that each time axioms are added, a new reasoner state is created. References to the previous state remain unchanged. This allows Whelk to answer queries concurrently, and also allows rapid classification of multiple datasets which build upon the same ontology Tbox, which can be classified ahead of time and stored. However, for basic classification of a single ontology, Whelk is much slower than ELK.

In addition to OWL EL classification, Whelk provides OWL RL and a subset of SWRL for reasoning on individuals.

## Status

Whelk is under development. It works, but its Scala API is in flux. A basic implementation of the OWL API OWLReasoner interface is included, but to make use of its immutability features, Whelk is best used from Scala.

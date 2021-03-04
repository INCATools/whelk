<img align="right" src="https://farm7.staticflickr.com/6205/6045158767_e70d43139d_m_d.jpg">

# Whelk

Whelk is an OWL reasoner based on the algorithm implemented in [ELK](https://github.com/liveontologies/elk-reasoner), as described in [The Incredible ELK](https://doi.org/10.1007/s10817-013-9296-3).

Whelk is implemented as an [immutable functional data structure](https://en.wikipedia.org/wiki/Purely_functional_data_structure), so that each time axioms are added, a new reasoner state is created. References to the previous state remain unchanged. This allows Whelk to answer queries concurrently, and also allows rapid classification of multiple datasets which build upon the same ontology Tbox, which can be classified ahead of time and stored. However, for basic classification of a single ontology, Whelk is much slower than ELK.

In addition to OWL EL classification, Whelk provides OWL RL and a subset of SWRL for reasoning on individuals.

## Use cases

Whelk is based on ELK, and ELK is much faster at classifying an ontology. Some reasons to use Whelk include:
- object property assertion materialization
- SWRL rules (class and object property atoms)
- OWL RL features for Abox
- some classification for unions in superclass position (e.g., infer least common subsumer of operands)
  - Example: 
    ```
    C SubClassOf B
    D SubClassOf B
    E EquivalentTo C or D
    then
    E SubClassOf B
    ```
  - *this feature is useful, but not guaranteed to be complete*
- extended support for Self restrictions, supporting rolification
- in application code, submitting many DL queries programmatically (Whelk is much faster at this)
- in application code, performing many DL queries in parallel, non-blocking
- in application code, storing a reasoning state (e.g., Tbox classification) and simultaneously extending it with multiple independent new axiom sets (e.g., Aboxes); or, quickly rolling back to a previous, saved reasoning state

## Status

Whelk is under development. It works, but its Scala API is in flux. A basic implementation of the OWL API OWLReasoner interface is included, but to make use of its immutability features, Whelk is best used from Scala.

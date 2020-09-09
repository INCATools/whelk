package org.geneontology.whelk

object Util {

  implicit class MapExtensions[K, V](val self: Map[K, Set[V]]) extends AnyVal {

    def |+|(other: Map[K, Set[V]]): Map[K, Set[V]] = {
      other.foldLeft(self) { case (accum, (key, set)) =>
        val current = accum.getOrElse(key, Set.empty[V])
        accum.updated(key, current ++ set)
      }
    }

  }

}

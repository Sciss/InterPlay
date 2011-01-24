package de.sciss.interplay

import collection.immutable.{SortedSet => ISortedSet}
import de.sciss.synth.Model

object AnalysisMarkers {
   case class Added( idx: Int )
}
class AnalysisMarkers extends Model {
   import AnalysisMarkers._

   private var set   = ISortedSet.empty[ Int ]
   private val sync  = new AnyRef

   def add( idx: Int ) {
      val isNew = sync.synchronized {
         val res = !set.contains( idx )
         if( res ) set += idx
         res
      }
      if( isNew ) dispatch( Added( idx ))
   }

   def all : ISortedSet[ Int ] = sync.synchronized( set )
}
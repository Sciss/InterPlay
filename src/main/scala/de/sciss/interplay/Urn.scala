package de.sciss.interplay

import collection.mutable.{ Buffer => MBuffer }

class Urn[ T ]( elements: T* ) {
   private val bag = MBuffer.empty[ T ]

   def next: T = {
      if( bag.isEmpty ) {
         bag.append( elements: _* )
      }
      val idx = Util.rand( bag.size )
      bag.remove( idx )
   }

   def take( n: Int ) : Seq[ T ] = {
      if( bag.size < n ) {
         bag.clear
         bag.append( elements: _* )
      }
      Seq.fill( n )( next )
   }
}
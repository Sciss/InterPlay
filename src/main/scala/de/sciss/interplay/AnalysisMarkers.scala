/*
 *  AnalysisMarkers.scala
 *  (InterPlay)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

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
/*
 *  Huellkurve.scala
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

import de.sciss.synth.{EnvShape, linShape, ConstEnvShape}

/**
 * Have this here until ScalaCollider's Env transparently supports RichNumbers as well
 */
object Huellkurve {
   case class Seg( dur: Float, targetLevel: Float, shape: ConstEnvShape = linShape )
}
case class Huellkurve( startLevel: Float, segments: Seq[ Huellkurve.Seg ]) {
   def levelAt( time: Float ) : Float = {
      // simple accumulative search
      var y0   = startLevel
      var y1   = startLevel
      var t0   =  0f
      var t1   = 0f
      var shape: ConstEnvShape = linShape

      segments.foreach { seg =>
         t0          = t1
         t1         += seg.dur
         y0          = y1
         y1          = seg.targetLevel
         shape       = seg.shape
         if( time <= t1 ) {
            val t = math.max( t0, math.min( t1, time ))
            val w = if( t1 > t0 ) (t - t0) / (t1 - t0) else 0f
            return shape.levelAt( w, y0, y1 )
         }
      }
      y1
   }
}
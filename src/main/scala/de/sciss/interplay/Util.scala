/*
 *  Util.scala
 *  (InterPlay)
 *
 *  Copyright (c) 2010-2011 Hanns Holger Rutz. All rights reserved.
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

import util.Random
import collection.generic.CanBuildFrom
import collection.immutable.{IndexedSeq => IIdxSeq}

/**
 * @version 0.11, 17-Aug-10
 */
object Util {
   val rnd = new Random()

   def exprand( lo: Double, hi: Double ) : Double = {
      lo * math.exp( math.log( hi / lo ) * rnd.nextDouble )
   }

   def rrand( lo: Double, hi: Double ) : Double = {
      rnd.nextDouble() * (hi - lo) + lo
   }

   // lo to hi
   def rrand( lo: Int, hi: Int ) : Int = {
      if( lo <= hi ) {
         rnd.nextInt( hi - lo + 1 ) + lo
      } else {
         rnd.nextInt( lo - hi + 1 ) + hi
      }
   }

   // 0 to i - 1 (0 until i)
   def rand( i: Int ) : Int= rnd.nextInt( i )

   def rand( d: Double ) : Double = rnd.nextDouble() * d

   def coin( w: Double ) : Boolean = rnd.nextDouble() < w

   def choose[ T ]( seq: Traversable[ T ]) : T = {
      val idxSeq = seq.toIndexedSeq
      idxSeq( rnd.nextInt( idxSeq.size ))
   }

   def wchoose[ T ]( seq: Traversable[ T ])( fun: T => Double ) : T = {
      val i    = rnd.nextDouble
      var sum  = 0.0
      seq find { e => sum += fun( e ); sum >= i } getOrElse seq.last
   }

   def scramble[ T, C <: Traversable[ T ], That ]( seq: C )( implicit bf: CanBuildFrom[ C, T, That ]) : That = {
      val b       = bf.apply()
      var remain  = seq.toIndexedSeq
      var sz      = remain.size
      while( sz > 0 ) {
         val idx  = rand( sz )
         b       += remain( idx )
         remain   = remain.patch( idx, Nil, 1 )
         sz      -= 1
      }
      b.result()
   }

   /**
    * Like scramble, but each item in the output is guaranteed to be _not_ at
    * its original index.
    */
   def scramble2[ T ]( seq: IIdxSeq[ T ]) : IIdxSeq[ T ] = {
      var remain  = seq
      var sz      = remain.size
      if( sz < 2 ) return remain // no way; could also throw an exception?
      val b       = seq.companion.newBuilder[ T ]
      var i       = 0
      while( sz > 2 ) {
         val idx0 = rand( sz - 1 )
         val idx  = if( idx0 < i ) idx0 else idx0 + 1
         b       += remain( idx )
         remain   = remain.patch( idx, Nil, 1 )
         sz -= 1; i += 1
      }
      // two are remaining
      val a1 = remain( 0 )
      val a2 = remain( 1 )
      val b1 = seq( i )
      val b2 = seq( i + 1 )
      if( a1 == b2 || a2 == b1 ) {
         b += a1
         b += a2
      } else if( a1 == b1 || a2 == b2 ) {
         b += a2
         b += a1
      } else { // free choice
         if( rand( 2 ) == 0 ) {
            b += a1
            b += a2
         } else {
            b += a2
            b += a1
         }
      }

      b.result()
   }

   def nextPowerOfTwo( x: Int ) : Int = {
      var y = 1
      while( y < x ) y <<= 1
      y
   }

   def nextPowerOfTwo( x: Long ) : Long = {
      var y = 1
      while( y < x ) y <<= 1
      y
   }

   def linlin( x: Double, srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) =
      (x - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo

   def linexp( x: Double, srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double) =
      math.pow( dstHi / dstLo, (x- srcLo) / (srcHi - srcLo) ) * dstLo
}
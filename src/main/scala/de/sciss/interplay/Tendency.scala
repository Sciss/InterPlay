/*
 *  Tendency.scala
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

import de.sciss.synth.{ConstEnvShape, curveShape, expShape, cubShape, sinShape, linShape, welchShape, stepShape, sqrShape, EnvShape}

object Tendency {
   sealed trait Distrib {
      def map( rnd: Float, lo: Float, hi: Float ) : Float
   }

   object Lin extends Distrib {
      def map( rnd: Float, lo: Float, hi: Float ) : Float = rnd * (hi - lo) + lo
   }

   object Exp extends Distrib {
      def map( rnd: Float, lo: Float, hi: Float ) : Float = {
         if( lo == hi ) lo else lo * math.exp( math.log( hi / lo ) * rnd ).toFloat
      }
   }

   implicit def symbolShape( sym: Symbol ) : ConstEnvShape = sym match {
      case 'welch => welchShape
      case 'step  => stepShape
      case 'sqr   => sqrShape
      case 'sin   => sinShape
      case 'lin   => linShape
      case 'exp   => expShape
      case 'cub   => cubShape
   }

   implicit def curved( curvature: Double ) : ConstEnvShape = new curveShape( curvature.toFloat )

   implicit def tuple2ToSeg( tup: (Double, (Double, Double)) ) : TimePoint =
      TimePoint( tup._1.toFloat, tup._2._1.toFloat, tup._2._2.toFloat, linShape )

   implicit def tuple3ToSeg[ S <% ConstEnvShape ]( tup: (Double, (Double, Double), S) ) : TimePoint =
      TimePoint( tup._1.toFloat, tup._2._1.toFloat, tup._2._2.toFloat, tup._3 )

   case class TimePoint( time: Float, lo: Float, hi: Float, shp: ConstEnvShape )

   def tend( name: String, d: Distrib, pts: TimePoint* ) : Tendency = {
      val durs    = pts.map( _.time ).sorted.sliding( 2, 1 ).map( tup => tup(1) - tup(0) ).toList
      val head    = pts.head
      val tail    = pts.tail
      val zipped  = durs.zip( tail )
      val envLo   = Huellkurve( head.lo, zipped.map( tup => Huellkurve.Seg( tup._1, tup._2.lo, tup._2.shp )))
      val envHi   = Huellkurve( head.hi, zipped.map( tup => Huellkurve.Seg( tup._1, tup._2.hi, tup._2.shp )))
      Tendency( name, envLo, envHi, d )
   }
}

case class Tendency( name: String, lo: Huellkurve, hi: Huellkurve, distrib: Tendency.Distrib ) {
   def at( time: Float ) : Float = {
println( "---deciding " + name + " at time " + time )
      val a = lo.levelAt( time )
      val b = hi.levelAt( time )
      val l = math.min( a, b )
      val h = math.max( a, b )
      val r = Util.rnd.nextFloat()
      distrib.map( r, l, h )
   }

   def loAt( time: Float ) : Float = lo.levelAt( time )
   def hiAt( time: Float ) : Float = hi.levelAt( time )

   lazy val overallLo = lo.overallLo
   lazy val overallHi = hi.overallHi

   def decide : Float = at( SoundProcesses.logicalTime().toFloat )
   def decideInt : Int = (decide + 0.5f).toInt
   def decideWithBounds : (Float, Float, Float) = {
      val tim = SoundProcesses.logicalTime().toFloat
      (at( tim ), loAt( tim ), hiAt( tim ))
   }
}
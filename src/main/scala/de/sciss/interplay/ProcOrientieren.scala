/*
 *  ProcOrientieren.scala
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

import de.sciss.synth._
import proc._
import ugen._
import DSL._
import InterPlay._
import Util._
import Tendency._
import Konvulsiv._

/**
 * Inserts a transformation which is a time-varying delay, creating
 * different glissandi across channels.
 */
object ProcOrientieren extends Process {
   import Process._

   val name    = "p-orient"
   val verbose = true

   val MIN_WAIT   = 30.0
   val MAX_WAIT   = 60.0

   val MIN_DLY    = 1.0
   val MAX_DLY    = 8.0
   val MIN_GLISS  = 6.0
   val MAX_GLISS  = 30.0
   val MIN_REENTRY= 45.0
   val MAX_REENTRY= 90.0
   val ALL_PROB   = 0.333

   private val KONVUL_NUM = konvul( name + "-num", (1.2, 1.8), (2, 4) ) { (t, k) =>
      Some( (t + 0.3, t + 0.5) -> (2, 4) )
   }

   private val TEND_RETRY  = tend( name + "-retry", Lin, 0.0 -> (0.3333, 0.3333), 0.95 -> (0.3333, 0.3333), (1.5, (0.05, 0.05), 'cub), (2.0, (0.3333, 0.3333), 'sin) )

   def init(  implicit tx: ProcTxn ) {
      filter( name ) {
//         val pdur    = pScalar( "dur", ParamSpec( MIN_REC, MAX_REC ), MIN_REC )
//         val ppos = pScalar( "pos", ParamSpec( 0, 1 ), 0 )
         graph { in =>
            val (sig, durs) = in.outputs.map( chan => {
               val bufFrames  = (rrand( MIN_DLY, MAX_DLY ) * SAMPLE_RATE).toInt
               val buf        = bufEmpty( bufFrames ).id
               val bufDur     = BufDur.ir( buf )
               val xfadeDur   = 1
               val fadeStart  = bufDur + xfadeDur
               val glissDur   = ExpRand( MIN_GLISS, MAX_GLISS )
               val dlyTime    = EnvGen.ar( Env( 1, List( EnvSeg( fadeStart, 1, stepShape ), EnvSeg( glissDur, 0, sinShape ))), levelScale = bufDur )
               val dly        = BufDelayL.ar( buf, chan, dlyTime )
               val res        = XFade2.ar( chan, dly, EnvGen.kr( Env( -1, List( EnvSeg( bufDur, -1, stepShape ), EnvSeg( xfadeDur, 1 )))))
               val totalDur   = fadeStart + glissDur
               (res, totalDur)
            }).unzip
            val maxDur = durs.foldLeft[ GE ]( 0 )( _ max _ )
            val me = Proc.local
            Done.kr( Line.kr( 0, 0, maxDur )).react {
               spawnAtomic( name + " filter removal" ) { implicit tx =>
                  ProcessHelper.stopAndDispose( me )
                  stopPlaying
                  reentry()
               }
            }
            sig: GE
         }
      }

      val t = exprand( MIN_WAIT, MAX_WAIT )
//      atomic( name + " start" ) { implicit tx =>
         inform( "Waiting for " + t + "s" )
         start( t )
//      }
   }

   private def start( dlyTime: Double )( implicit tx: ProcTxn ) {
      startThinking
      delay( dlyTime ) {
         // XXX not clear why, but atomic instead of spawnAtomic here
         // creates timeouts
         spawnAtomic( name + " start delay done" ) { implicit tx =>
            stopThinking
            if( keepGoing ) {
               inform( "Playing" )

               def funkyShit( implicit tx: ProcTxn ) : Boolean = {
                  val pt = if( coin( ALL_PROB )) ReplaceAll else ReplaceInternal
                  val can = canReplaceTail( pt )
                  if( can ) {
                     val filtFact = factory( name )
                     val p = filtFact.make
                     replaceTail( p, point = pt )
                  }
                  can
               }

               if( funkyShit ) {
                  startPlaying
                  for( n <- 1 until KONVUL_NUM.decideOrElse( 1 )) {
                     spawnAtomic( name + " konvulsiv " + n )( funkyShit( _ ))
                  }
               } else {
                  reentry( TEND_RETRY.decide )
               }
            }
         }
      }
   }

   private def reentry( factor: Double = 1.0 )( implicit tx: ProcTxn ) {
      val t = exprand( MIN_REENTRY, MAX_REENTRY ) * factor
      inform( "Re-entry after " + t + "s" )
      start( t )
   }
}
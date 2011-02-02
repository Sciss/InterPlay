/*
 *  ProccHoeren.scala
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
import SoundProcesses._

/**
 * Listens to individual channels, and when they stay too quiet after a climax,
 * injects material from the live buffer around detected attacks. Tries to
 * sync with attacks on other channels.
 */
object ProcHoeren extends Process {
   import Process._

   val name    = "p-hoeren"
   val verbose = true

   private val anaName = name + "-ana"

//   lazy val MIN_WAIT      = (liveDur - 1) * 60
//   lazy val MAX_WAIT      = liveDur * 60

   val STICKY_DUR = 10

   def init( implicit tx: ProcTxn ) {
      filter( anaName ) {
         val ptup    = pControl( "up", ParamSpec( 0, 64 ), 32 )
         val ptdown  = pControl( "down", ParamSpec( 0, 64 ), 32 )
         graph { in =>
            val threshUp = ptup.kr
            val threshDown = ptdown.kr
            in.outputs.zipWithIndex.foreach { tup =>
               val (chSig, ch) = tup
               val loudness   = Loudness.kr( FFT( bufEmpty( 1024 ).id, chSig ))
               val lagged     = LagUD.kr( loudness, 0, 2 )
               val schmidt    = Schmidt.kr( lagged, threshUp, threshDown )
               val trig       = -HPZ1.kr( schmidt ) // goes >0 if schmidt goes from hi to lo
               trig.react { ProcTxn.spawnAtomic { implicit tx => dang( ch )}}
            }
            val me = Proc.local
            Done.kr( Line.kr( 0, 0, STICKY_DUR )).react {
               ProcTxn.spawnAtomic { implicit tx =>
                  ProcessHelper.stopAndDispose( me )
                  reentry
               }
            }
            in // thru
         }
      }

      gen( name ) {
         val ppos    = pScalar( "pos", ParamSpec( 0, 1 ), 0 )
         val pdur    = pScalar( "dur", ParamSpec( 0.1, 60.0 ), TEND_STEADY.overallLo )
         val pspeed  = pScalar( "speed", ParamSpec( 1.0/8, 8, ExpWarp ), 1 )
         graph {
            val speed      = pspeed.ir
            val dur0       = pdur.ir
            val dur        = dur0 / speed
            val atk        = 0.01
//            val rls        = (dur - atk).min( 2 ).max( 0 )
            val rls        = (dur - atk - 1).max( 0 )
            val sus        = (dur - rls - atk).max( 0 )
            val freq       = speed / dur
            val sig        = PlayBuf.ar( liveBuf.numChannels, liveBuf.id, speed, startPos = ppos.ir * BufFrames.ir( liveBuf.id ), loop = 0 )
            val env        = EnvGen.ar( Env.linen( atk, sus, rls ))
            val me         = Proc.local
            Done.kr( env ).react {
               ProcTxn.spawnAtomic { implicit tx =>
                  Process.removeAndDispose( org.diff, 0.1 )
//                  stopPlaying
//                  reentry()
               }
            }
            val cmp = Compander.ar( sig, sig, thresh = 0.5, ratioBelow = 1.0/4, ratioAbove = 1, attack = 0.01, release = 1.0 )   // compress low stuff
            cmp * env
         }
      }

      delay( liveDur * 60 )( ProcTxn.atomic( start ))
   }

   private def start( implicit tx: ProcTxn ) {
      error( "NOT YET" )
   }

   private def reentry( implicit tx: ProcTxn ) {
      error( "NOT YET" )
   }

   private def dang( ch: Int )( implicit tx: ProcTxn ) {
      inform( "dang " + ch )
      val ms   = anaMarkers.all.toIndexedSeq
      if( ms.isEmpty ) return
      val m    = Util.choose( ms )
      val mi   = ms.indexOf( m )
      val m2   = if( mi == ms.size - 1 ) anaClientBuf.framesWritten else ms( mi + 1 )

      val pos  = framesToPos( math.max( 0, m - 1 ))
      val dur  = math.max( 1.0, frameToSecs( m2 ) - frameToSecs( m ))

      val p    = factory( name ).make
      val d    = factory( "O-one" ).make
      p.control( "pos" ).v = pos
      p.control( "dur" ).v = dur
      // XXX speed? tend!
      d.control( "idx" ).v = ch
      p ~> d
      addTail( d )
   }
}
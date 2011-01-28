/*
 *  ProcGleichgewichten.scala
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

import InterPlay._
import SoundProcesses._
import Util._
import de.sciss.synth._
import proc._
import ugen._
import DSL._

object ProcGleichgewichten extends Process {
   import Process._

   val name          = "p-gleich"
   val verbose       = true

   val MIN_WAIT      = 25.0
   val MAX_WAIT      = 40.0
   val MIN_STEADY    = 1.0
   val MAX_STEADY    = 2.0
//   val MAX_RESULTS   = 20
   val MIN_FADE      = 1.0
   val MAX_FADE      = 10.0

   def init(  implicit tx: ProcTxn ) {
      val genFact = gen( name ) {
         val ppos    = pAudio( "pos", ParamSpec( 0, 1 ), 0 )
         val pdur    = pAudio( "dur", ParamSpec( MIN_STEADY, MAX_STEADY ), MIN_STEADY )
         val pspeed  = pAudio( "speed", ParamSpec( 1.0/8, 8, ExpWarp ), 1 )
         graph {
            val speed      = pspeed.ar
            val dur        = pdur.ar
            val freq       = speed / dur
            val startFrame = ppos.ar * BufFrames.ir( liveBuf.id )
            val stopFrame  = startFrame + dur * SampleRate.ir
            val frame      = LFSaw.ar( freq, -1 ).linlin( -1, 1, startFrame, stopFrame ) % BufFrames.ir( liveBuf.id )
            val amp        = (frame - startFrame).min( stopFrame - frame ).min( 48 ) / 48
            val sig        = BufRd.ar( liveBuf.numChannels, liveBuf.id, frame, interp = 2 )
            sig * amp
         }
      }

      val diffFact = factory( "O-one" )

      val steady     = rrand( MIN_STEADY, MAX_STEADY )
      val waitTime   = rrand( MIN_WAIT, MAX_WAIT )
      inform( "waitForAnalysis " + waitTime )

      startThinking
      waitForAnalysis( waitTime ) {
         inform( "searchAnalysis " + steady )
         searchAnalysis( steady,
            maxResults = MASTER_NUMCHANNELS,
            frameMeasure = minFlat( _ ), integMeasure = worstFlat( _ )) { res =>

            inform( "result " + res )
            ProcTxn.spawnAtomic { implicit tx => res.zipWithIndex.foreach { tup =>
               stopThinking
               startPlaying

               val (smp, idx) = tup
               val p = genFact.make
               p.control( "dur" ).v = exprand( MIN_STEADY, steady )
               // DDD p.control( "speed" ).v = fluctuate( phase )
//               println( "juuu. measure = " + smp.measure )
               // measure is typically between 0.0 (maximally flat) and 0.5
               p.control( "pos" ).v = framesToPos( smp.idx )
               val d = diffFact.make
               d.control( "idx" ).v = idx
               p ~> d
               ProcHelper.playNewDiff( rrand( MIN_FADE, MAX_FADE ), d )
            }}
         }
      }
   }

   private def minFlat( buf: Array[ Float ]) : Float = {
      var f0   = math.abs( buf( 0 ))
      val n    = buf.size
      var meas = 0.0f
      var i = 1; while( i < n ) {
         val f1 = math.abs( buf( i ))
         if( f1 > f0 ) {
            meas += (f1 - f0) / f1
         } else if( f0 > f1 ) {
            meas += (f0 - f1) / f0
         }
         f0 = f1
      i += 1 }
      meas / n
   }

   private def worstFlat( buf: Array[ Float ]) : Float = {
      var m = buf( 0 )
      var i = 1; while( i < buf.size ) {
         val m1 = buf( i )
         if( m1 > m ) m = m1
      i += 1 }
      m
   }

//   def start( implicit tx: ProcTxn ) {
//
//   }
//
//   def stop(  implicit tx: ProcTxn ) {
//
//   }
}
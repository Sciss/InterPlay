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
import Tendency._
import collection.breakOut

/**
 * Searches for steady flat (noisy / little resonance) spectra,
 * plays short loops back directly from the buffer
 */
object ProcGleichgewichten extends Process {
   import Process._

   val name          = "p-gleich"
   val verbose       = true

   val MIN_WAIT      = 25.0
   val MAX_WAIT      = 40.0

   val TEND_STEADY   = tend( name + "-steady", Lin, 0.0 -> (1.0, 2.0), 1.0 -> (1.5, 3.0), 2.0 -> (1.0, 3.5) )

//   val MIN_STEADY    = 1.0
//   val MAX_STEADY    = 2.0
//   val MAX_RESULTS   = 20

   val TEND_FADE     = tend( name + "-fade", Lin, 0.0 -> (1.0, 10.0), 2.0 -> (1.0, 10.0) )

   val TEND_SPEED    = tend( name + "-speed", Exp, 0.0 -> (1.0, 1.0), 1.0 -> (0.9715, 1.0293), (2.0, (0.5, 1.0), 'welch) )

//   val MIN_FADE      = 1.0
//   val MAX_FADE      = 10.0

   val TEND_PLAY     = tend( name + "-play", Exp, 0.0 -> (45.0, 70.0), 0.9 -> (40.0, 50.0), 2.0 -> (35.0, 50.0) )

   val TEND_ALGO     = tend( name + "-algo", Lin, 0.0 -> (0.0, 0.0), 1.0 -> (0.0, 0.0), 2.0 -> (1.0, 1.0) )
   private val algos = Array( worstFlat( _ ), bestFlat( _ ))

//   val MIN_PLAY      = 60.0
//   val MAX_PLAY      = 120.0

   val TEND_REENTRY  = tend( name + "-reentry", Exp, 0.0 -> (90.0, 135.0), 0.9 -> (90.0, 120.0), 1.5 -> (60.0, 60.0), 1.6 -> (999.9, 999.9) )

//   val TEND_COMP     = tend( name + "-comp", Exp, 0.0 -> (1.0, 1.0), 1.0 -> (1.4, 1.7), 1.5 -> (2.0, 2.0), 2.0 -> (1.4, 1.7) )
//   val TEND_COMP     = tend( name + "-comp", Exp, 0.0 -> (1.5, 1.5), 1.0 -> (2.0, 2.5), 1.5 -> (3.0, 3.0), 2.0 -> (1.5, 1.5) )
   val TEND_COMP     = tend( name + "-comp", Exp, 0.0 -> (1.2, 1.3), 1.0 -> (1.8, 2.2), 1.5 -> (3.0, 3.0), 2.0 -> (1.5, 1.5) )

//   val MIN_REENTRY   = 45.0
//   val MAX_REENTRY   = 90.0

   def init( implicit tx: ProcTxn ) {
      gen( name ) {
         val ppos    = pAudio( "pos", ParamSpec( 0, 1 ), 0 )
         val pdur    = pAudio( "dur", ParamSpec( TEND_STEADY.overallLo, TEND_STEADY.overallHi ), TEND_STEADY.overallLo )
         val pspeed  = pAudio( "speed", ParamSpec( 1.0/8, 8, ExpWarp ), 1 )
         val pcomp   = pControl( "comp", ParamSpec( 1.0, 3.0, ExpWarp ), 1 )
         graph {
            val speed      = pspeed.ar
            val dur        = pdur.ar
            val freq       = speed / dur
            val startFrame = ppos.ar * BufFrames.ir( liveBuf.id )
            val stopFrame  = startFrame + dur * SampleRate.ir
            val frame      = LFSaw.ar( freq, -1 ).linlin( -1, 1, startFrame, stopFrame ) % BufFrames.ir( liveBuf.id )
            val amp        = (frame - startFrame).min( stopFrame - frame ).min( 48 ) / 48
            val sig        = BufRd.ar( liveBuf.numChannels, liveBuf.id, frame, interp = 2 )
            val sig1       = sig * amp
            Compander.ar( sig1, sig1, ratioBelow = pcomp.kr.reciprocal )
         }
      }

      start
   }

   def start( implicit tx: ProcTxn ) {
      val waitTime   = rrand( MIN_WAIT, MAX_WAIT )
      inform( "waitForAnalysis " + waitTime )
      startThinking
      waitForAnalysis( waitTime )( atomic( name + " waitForAnalysis done" )( analysisReady( _ )))
   }

//   private val tail = Ref( List.empty[ Proc ])

   private val usedRef = Ref( Set.empty[ Int ])

   private def analysisReady( implicit tx: ProcTxn ) {
      val (steady, steadyLo, steadyHi) = TEND_STEADY.decideWithBounds
      inform( "searchAnalysis " + steady )
      val algo = algos( TEND_ALGO.decideInt )
      searchAnalysis( steady,
         maxResults = MASTER_NUMCHANNELS + usedRef().size,
         frameMeasure = minFlat( _ ), integMeasure = algo ) { res0 =>

         inform( "searchAnalysis done" )
         spawnAtomic( name + " searchAnalysis done" ) { implicit tx =>
            val usedIter = usedRef().iterator
            var resS = res0.map( _.idx )
            while( resS.size > MASTER_NUMCHANNELS && usedIter.hasNext ) {
               resS = resS - usedIter.next
            }
            resS = resS.take( MASTER_NUMCHANNELS )

            stopThinking

            if( keepGoing ) {
               inform( "playing " + resS )
               usedRef.transform( _ ++ resS )
               startPlaying

               val speed = TEND_SPEED.decide
               val tail: List[ Proc ] = resS.zipWithIndex.map( tup => {
                  val (smpIdx, idx) = tup
                  val genFact = factory( name )
                  val p = genFact.make
                  p.control( "dur" ).v = exprand( steadyLo, steady )
                  // DDD p.control( "speed" ).v = fluctuate( phase )
      //               println( "juuu. measure = " + smp.measure )
                  // measure is typically between 0.0 (maximally flat) and 0.5
                  p.control( "pos" ).v = framesToPos( smpIdx )
                  p.control( "comp" ).v = TEND_COMP.decide
                  p.control( "speed" ).v = speed
                  val diffFact = factory( "O-one" )
                  val d = diffFact.make
                  d.control( "idx" ).v = idx
                  p ~> d
                  addTail( d, TEND_FADE.decide )
                  d
               })( breakOut )

               if( resS.nonEmpty ) {
                  val playTime = TEND_PLAY.decide
                  inform( "Playing for " + playTime + "s" )
                  delay( playTime ) {
                     tail.foreach { proc =>
                        spawnAtomic( name + " remove one tail" ) { implicit tx =>
                           removeAndDispose( name + " play done", proc, TEND_FADE.decide )
                        }
                        spawnAtomic( name + " stopping" ) { implicit tx =>
                           inform( "Stopping" )
                           stopPlaying
                        }
                     }
                  }
               }
               reentry
            }
         }
      }
   }

   private def reentry( implicit tx: ProcTxn ) {
      val reentryTime = TEND_REENTRY.decide
      inform( "Re-entry after " + reentryTime + "s" )
//      tail.swap( Nil ).foreach( removeAndDispose( _, TEND_FADE.decide ))
//      stopPlaying
      delay( reentryTime )( spawnAtomic( name + " reentry done" )( start( _ )))
   }

   private def minFlat( buf: Array[ Float ]) : Float = {
      var f0   = math.abs( buf( 0 ))
      val n    = buf.length
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
      -meas / n   // large changes (low flatness) is punished
   }

   // XXX TODO
   private def worstFlat( buf: Array[ Float ]) : Float = {
      var m = buf( 0 )
      var i = 1; while( i < buf.length ) {
         val m1 = buf( i )
         if( m1 > m ) m = m1
      i += 1 }
      m
   }

   // XXX TODO
   private def bestFlat( buf: Array[ Float ]) : Float = {
      var m = buf( 0 )
      var i = 1; while( i < buf.length ) {
         val m1 = buf( i )
         if( m1 < m ) m = m1
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
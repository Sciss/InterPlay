/*
 *  ProcSchmecken.scala
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

import java.io.File

import de.sciss.interplay.Konvulsiv._
import de.sciss.interplay.SoundProcesses._
import de.sciss.interplay.Tendency._
import de.sciss.interplay.Util._
import de.sciss.synth
import de.sciss.synth._
import de.sciss.synth.proc.DSL._
import de.sciss.synth.proc._
import de.sciss.synth.ugen._

/**
 * Records part of the sum signal and runs it through FScape.
 * At the moment this only uses Wavelet decomposition.
 * Reacts to Gleichgewichten becoming active (as that results
 * in nice repetitive patterns).
 */
object ProcSchmecken extends Process {
   import Process._

   val name    = "p-schmeck"
   val verbose = true

   val MIN_WAIT   = 10.0
   val MAX_WAIT   = 30.0
   val MIN_REC    = 10.0
   val MAX_REC    = 45.0
//   val ALL_PROB   = 0.333

   val MIN_KONVULDLY = 0.1
   val MAX_KONVULDLY = 10.0
   val MIN_RETRY     = 1.0
   val MAX_RETRY     = 4.0

   private val KONVUL_NUM = konvul( name + "-num", (1.2, 1.8), (4, 7) ) { (t, k) =>
      Some( (t + 0.15, t + 0.3) -> (4, 7) )
   }

   private val TEND_FASTRETRY = tend( name + "-fastretry", Lin, 0.0 -> (0.0, 0.0), 0.85 -> (0.0, 0.0), 1.3 -> (1.0, 1.0), (2.0, (0.0, 0.0), 'sin) )
   private val TEND_ALLPROB   = tend( name + "-allprob", Lin, 0.0 -> (0.3333, 0.3333), 0.85 -> (0.3333, 0.3333), 1.3 -> (1.0, 1.0), (2.0, (0.3333, 0.33333), 'sin) )

   private val TEND_RECTIME   = tend( name + "-rectime", Lin, 0.0 -> (10.0, 45.0), 1.0 -> (10.0, 45.0), 2.0 -> (20.0, 45.0) )
   private val TEND_AMP       = tend( name + "-amp", Lin, 0.0 -> (1.0, 1.0), 1.0 -> (1.0, 1.0), (1.5, (2.0, 2.0), 'sin), (2.0, (1.0, 1.0), 'sin) )

   private val anaNameF = name + "-anaf"
   private val anaNameD = name + "-anad"

   def init(  implicit tx: ProcTxn ) {
      def flonky( in: In, pdur: ProcParamScalar, ppos: ProcParamScalar ) {
         val recPath    = File.createTempFile( "sum", ".aif" ) // , sumPath
         val b          = bufRecord( recPath.getAbsolutePath, in.numChannels)
         val dur        = pdur.ir
         val env        = EnvGen.kr( Env.linen( 0.1, dur - 1.1, 1.0 ))
         val phase      = Line.kr( 0, 1, dur )
         DiskOut.ar( b.id, in * env )
         val me         = Proc.local
         1.react( phase ) { data =>
            val Seq( ph ) = data
            spawnAtomic( name + " gen pos update" ) { implicit tx => me.control( "pos" ).v = ph }
         }
         Done.kr( phase ).react {
            spawnAtomic( name + " gen removal" ) { implicit tx =>
               ProcessHelper.stopAndDispose( me )
               if( keepGoing ) {
                  inform( "injectWavelet" )
                  FScape.injectWavelet( recPath, TEND_AMP.decide )
               }
            }
         }
      }

      filter( anaNameF ) {
         val pdur    = pScalar( "dur", ParamSpec( MIN_REC, MAX_REC ), MIN_REC )
         val ppos    = pScalar( "pos", ParamSpec( 0, 1 ), 0 )
         graph { in: In =>
            flonky( in, pdur, ppos )
            in // thru
         }
      }

      diff( anaNameD ) {
         val pdur    = pScalar( "dur", ParamSpec( MIN_REC, MAX_REC ), MIN_REC )
         val ppos    = pScalar( "pos", ParamSpec( 0, 1 ), 0 )
         graph { in: In =>
            flonky( in, pdur, ppos )
            0.0
         }
      }

      ProcGleichgewichten.addListener( new Listener {
         var oldState = State( false )
         def updated( u: Update ) {
            if( u.state.valid && u.state.playing && !oldState.playing ) {
               val t = exprand( MIN_WAIT, MAX_WAIT )
               atomic( name + " gleichgew listener" ) { implicit tx =>
                  startThinking
                  makeDemDelay( t )
               }
            }
            oldState = u.state
         }
      })
   }

   private def delayDone( implicit tx: ProcTxn ) {
      stopThinking

      def funkyShit( implicit tx: ProcTxn ) : Boolean = {
         if( liveActive ) {
            val pt   = if( coin( TEND_ALLPROB.decide )) ReplaceAll else ReplaceInternal
            val can  = canReplaceTail( pt )
            if( can ) {
               val p = factory( anaNameF ).make
               p.control( "dur" ).v = TEND_RECTIME.decide // rrand( MIN_REC, MAX_REC )
               p.control( "pos" ).v = 0.0
               replaceTail( p, point = pt )
            }
            can
         } else {
            val p = factory( anaNameD ).make
            collAll ~> p
            p.play
            true
         }
      }

      val ok = funkyShit
      inform( "starting analysis" + ok )
      if( ok ) {
         startPlaying    // XXX stopPlaying missing
         for( n <- 1 until KONVUL_NUM.decideOrElse( 1 )) {
            delay( exprand( MIN_KONVULDLY, MAX_KONVULDLY ))( spawnAtomic( name + " konvul " + n ) { implicit tx =>
               inform( "konvul " + n )
               funkyShit
            })
         }
      } else if( TEND_FASTRETRY.decideInt > 0 ) {
         val t = exprand( MIN_RETRY, MAX_RETRY )
         makeDemDelay( t )
      }
   }

   private def makeDemDelay( t: Double )( implicit tx: ProcTxn ) {
      inform( "delay for" + t + "s" )
      delay( t )( spawnAtomic( name + " delay done" )( delayDone( _ )))
   }
}
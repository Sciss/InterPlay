/*
 *  ProcKoerper.scala
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
import SoundProcesses._
import java.io.File
import de.sciss.fscape.FScapeJobs

object ProcKoerper extends Process {
   import Process._

   val verbose = true
   val name    = "p-koerp"

//   val REC_MIN = 60.0
//   val REC_MAX = 90.0
   val REC_MIN = 45.0
   val REC_MAX = 60.0
   val MIN_WAIT   = liveDur * 60 * 0.4 // 5
   val MAX_WAIT   = MIN_WAIT

   private val anaName = name + "-ana"

   private val orgRef = Ref( Map.empty[ Proc, Org ])
   private case class Org( dur: Double, path1: Option[ String ], path2: Option[ String ])

   def init( implicit tx: ProcTxn ) {
      diff( anaName ) {
         val pdur = pScalar( "dur", ParamSpec( REC_MIN, REC_MAX ), REC_MIN )
         graph { in =>
            val recPath    = File.createTempFile( "koerp", ".aif" ).getAbsolutePath
            val b          = bufRecord( recPath, in.numOutputs )
            val me         = Proc.local
            atomic( name + " setting org" )( implicit tx => orgRef.transform { map =>
               val org = map( me )
               val org1 = if( org.path1.isEmpty ) org.copy( path1 = Some( recPath )) else org.copy( path2 = Some( recPath ))
               map + (me -> org1)
            })
            val dur        = pdur.ir
            val env        = EnvGen.kr( Env.linen( 0.1, dur - 1.1, 1.0 ))
            val phase      = Line.kr( 0, 1, dur )
            val in1        = LeakDC.ar( in )
            DiskOut.ar( b.id, in1 * env )
//            1.react( phase ) { data =>
//               val Seq( ph ) = data
//               spawnAtomic( name + " gen pos update" ) { implicit tx => me.control( "pos" ).v = ph }
//            }
            Done.kr( phase ).react {
               spawnAtomic( name + " rec done" ) { implicit tx =>
                  val mo = orgRef().get( me )
                  (mo.flatMap( _.path1 ), mo.flatMap( _.path2 )) match {
                     case (Some( _ ), None ) =>
                        inform( "first rec done" )
                        me.stop
                        me.play
                     case (Some( p1 ), Some( p2 )) =>
                        inform( "second rec done" )
                        orgRef.transform( _ - me )
                        ProcessHelper.stopAndDispose( me )
                        recsDone( mo.get.dur, p1, p2 )   // mo.get.dur -- very ugly
                     case x =>
                        inform( "wooo, wrong org? " + x, force = true )
                        orgRef.transform( _ - me )
                        ProcessHelper.stopAndDispose( me )
                  }
               }
            }
            0.0
         }
      }
      waitForAnalysis( frameToSecs( 1 ))( spawnAtomic( name + " live started" )( liveStarted( _ )))
   }

   private def liveStarted( implicit tx: ProcTxn ) {
      val t = Util.rrand( MIN_WAIT, MAX_WAIT )
      inform( "delay for " + t + "s" )
      delay( t )( spawnAtomic( name + " delay done" )( delayDone( _ )))
   }

   private def delayDone( implicit tx: ProcTxn ) {
      inform( "delay done" )
      val p    = factory( anaName ).make
      val dur  = Util.exprand( REC_MIN, REC_MAX )
      p.control( "dur" ).v = dur
      orgRef.transform( _ + (p -> Org( dur, None, None )))
      collAll ~> p
      p.play
   }

   private def recsDone( dur: Double, path1: String, path2: String )( implicit tx: ProcTxn ) {
//      inform( "recs done" )
      val numIRs = (dur / 5 + 0.5).toInt
      tx.afterCommit { tx =>
         val tmpPath = File.createTempFile( "fsc", ".aif" ).getAbsolutePath()
         val outPath = File.createTempFile( "fsc", ".aif" ).getAbsolutePath()
         val docBleach = FScapeJobs.Bleach( path2, None, tmpPath, length = 256, feedback = "-50.0dB" )
         val docConv = FScapeJobs.Convolution( path1, tmpPath, outPath, numIRs = numIRs, winStep = "0.5s", normIRs = true, minPhase = false /* true */)
         FScape.fsc3.processChain( name, docBleach :: docConv :: Nil )( success => spawnAtomic( name + " fscape done" )( fscapeDone( success, outPath )( _ )))
      }
   }

   private def fscapeDone( success: Boolean, outPath: String )( implicit tx: ProcTxn ) {
      if( success ) {
         if( !keepGoing ) return
         inform( "inject" )
         FScape.inject( new File( outPath ))
      } else {
         inform( "fscape failed", force = true )
      }
   }
}
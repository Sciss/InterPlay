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

import de.sciss.synth
import synth._
import ugen._
import proc._
import DSL._
import Util._
import java.io.File
import InterPlay._
import SoundProcesses._

object ProcSchmecken extends Process {
   import Process._

   val MIN_WAIT   = 10.0
   val MAX_WAIT   = 30.0
   val MIN_REC    = 10.0
   val MAX_REC    = 45.0
   val ALL_PROB   = 0.333

   val name    = "p-schmeck"
   val verbose = true

   def init(  implicit tx: ProcTxn ) {
      val genFact = filter( name ) {
         val pdur    = pScalar( "dur", ParamSpec( MIN_REC, MAX_REC ), MIN_REC )
         val ppos    = pScalar( "pos", ParamSpec( 0, 1 ), 0 )
         val sumPath = new File( REC_PATH, "sum" )
         graph { in =>
            val recPath    = File.createTempFile( "sum", ".aif", sumPath )
//            val in         = LeakDC.ar( InFeedback.ar( masterBus.index, masterBus.numChannels ))
            val b          = bufRecord( recPath.getAbsolutePath, in.numOutputs )
            val dur        = pdur.ir
            val env        = EnvGen.kr( Env.linen( 0.1, dur - 1.1, 1.0 ))
            val phase      = Line.kr( 0, 1, dur )
            DiskOut.ar( b.id, in * env )
            val me         = Proc.local
            1.react( phase ) { data =>
               val Seq( ph ) = data
               ProcTxn.spawnAtomic { implicit tx => me.control( "pos" ).v = ph }
            }
            Done.kr( phase ).react {
               ProcTxn.spawnAtomic { implicit tx =>
                  ProcHelper.stopAndDispose( me )
                  FScape.injectWavelet( recPath )
               }
            }
//            Silent.ar
            in // thru
         }
      }

      ProcGleichgewichten.addListener( new Listener {
         var oldState = State( false )
         def updated( u: Update ) {
            if( u.state.valid && u.state.playing && !oldState.playing ) {
               val t = exprand( MIN_WAIT, MAX_WAIT )
               ProcTxn.atomic { implicit tx =>
                  startThinking
                  delay( t ) {
                     ProcTxn.atomic { implicit tx =>
                        stopThinking
                        val pt = if( coin( ALL_PROB )) ReplaceAll else ReplaceInternal
                        if( canReplaceTail( pt )) {
                           val p = genFact.make
                           p.control( "dur" ).v = rrand( MIN_REC, MAX_REC )
                           p.control( "pos" ).v = 0.0
                           replaceTail( p, point = pt )
                           startPlaying    // XXX stopPlaying missing
                        }
                     }
                  }
               }
            }
            oldState = u.state
         }
      })
   }
}
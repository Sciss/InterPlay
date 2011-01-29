/*
 *  Process.scala
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
import de.sciss.synth.Model
import collection.immutable.{IndexedSeq => IIdxSeq, SortedSet => ISortedSet}
import actors.Actor
import java.util.{TimerTask, Timer}
import de.sciss.synth.proc.{DSL, Proc, TxnModel, Ref, ProcTxn}
import DSL._

object Process {
   val verbose = true
   val all = List( ProcSehen, ProcHoeren, ProcRiechen, ProcSchmecken, ProcTasten, ProcOrientieren, ProcGleichgewichten )
//   lazy val map: Map[ String, Process ] = all.map( p => p.name -> p )( collection.breakOut )

   private val actor = new Actor { def act = loop { react {
      case d: Do => d.perform
   }}}

   def init( implicit tx: ProcTxn ) {
      actor.start
      all.foreach( _.init )
   }

   def secsToFrames( secs: Double ) = (secs * (SAMPLE_RATE / anaWinStep)).toInt
   def framesToPos( idx: Int )      = idx.toDouble / (anaClientBuf.numFrames - 1)

   private lazy val timer = new Timer( true )

   def delay( dur: Double )( thunk: => Unit ) : TimerTask = {
      val res = new TimerTask {
         def run = thunk
      }
      timer.schedule( res, (dur * 1000).toLong )
      res
   }

   def addTail( p: Proc, fadeTime: Double = 0.0 )( implicit tx: ProcTxn ) {
//      ProcHelper.playNewDiff( fadeTime, p )
      p ~> pDiffThru
      if( fadeTime > 0 ) xfade( fadeTime ) { p.play } else p.play
   }

   def replaceTail( p: Proc, fadeTime: Double = 0.0 )( implicit tx: ProcTxn ) {
      val oldIn   = pDiffThru.audioInput( "in" )
      val es      = oldIn.edges
      val newIn   = p.audioInput( "in" )
      es.foreach { e =>
         e.out ~/> oldIn
         e.out ~> newIn
      }
      if( fadeTime > 0 ) {
         p.bypass
         p ~> pDiffThru
         p.play
         xfade( fadeTime ) { p.engage }
      } else {
         p ~> pDiffThru
         p.play
      }
   }

   def waitForAnalysis( minDur: Double )( thunk: => Unit ) {
      val minFrames = secsToFrames( minDur )
      if( anaClientBuf.framesWritten >= minFrames ) {
         thunk
      } else {
         lazy val l: Model.Listener = {
            case AnalysisBuffer.FrameUpdated( idx, last ) => if( idx >= minFrames ) {
               anaClientBuf.removeListener( l )
               thunk
            }
         }
         anaClientBuf.addListener( l )
      }
   }

   private def inform( what: => String ) = if( verbose ) println( "Process : " + what )

   def searchAnalysis( timeInteg: Double, maxResults: Int = 20, frameMeasure: Array[ Float ] => Float,
                       integMeasure: Array[ Float ] => Float, rotateBuf: Boolean = false )
                     ( fun: ISortedSet[ Sample ] => Unit ) {
      require( maxResults > 0, "maxResults must be > 0, but is " + maxResults )
      spawn {
         inform( "searchAnalysis started" )
         val frameInteg = secsToFrames( timeInteg )
         val buf        = anaClientBuf
         val chanBuf    = new Array[ Float ]( buf.numChannels )
         val timeBuf    = new Array[ Float ]( frameInteg )
         val numFrames  = buf.framesWritten - timeInteg + 1
         var res        = ISortedSet.empty[ Sample ]( sampleOrd )
         var resCnt     = 0

         def karlheinz( idx: Int ) {
            val m = integMeasure( timeBuf )
            if( resCnt < maxResults ) {
               res += Sample( idx, m )
               resCnt += 1
            } else if( res.last.measure > m ) {
               res = res.dropRight( 1 ) + Sample( idx, m )
            }
         }

         if( numFrames > 0 ) {
            var x = 0; while( x < frameInteg ) {
               buf.getFrame( 0, chanBuf )
               timeBuf( x ) = frameMeasure( chanBuf )
            x += 1 }
            karlheinz( 0 )
         }
         var off = 1; while( off < numFrames ) {
            val fm = frameMeasure( buf.getFrame( off, chanBuf ))
            if( rotateBuf ) {
               System.arraycopy( timeBuf, 1, timeBuf, 0, frameInteg - 1 )
               timeBuf( 0 ) = fm
            } else {
               timeBuf( (off - 1) % frameInteg ) = fm
            }
            karlheinz( off )
         off += 1 }

         inform( "searchAnalysis done" )
         fun( res )
      }
   }

   private def spawn( thunk: => Unit ) = actor ! Do( thunk )

   private object Do { def apply( thunk: => Unit ) = new Do( thunk )}
   private class Do( thunk: => Unit ) { def perform = thunk }
   case class Sample( idx: Int, measure: Float ) extends Ordered[ Sample ] {
       def compare( that: Sample ) : Int = idx.compare( that.idx )
   }
   private val sampleOrd = Ordering.ordered[ Sample ]

   case class State( valid: Boolean, thinking: Boolean = false, playing: Boolean = false )
   case class Update( p: Process, state: State )

   type Listener = TxnModel.Listener[ Process.Update ]
}

trait Process extends TxnModel[ Process.Update ] {
   proc =>

   import Process._

//   private val activeRef = Ref( false )
   private val stateRef = Ref( State( true ))

   def name : String
   def verbose : Boolean

   def init(  implicit tx: ProcTxn ) : Unit

   protected def inform( what: => String ) = if( verbose ) println( name + ": " + what )

//   def start( implicit tx: ProcTxn ) {
//      val st = state
//      if( !st.active ) {
//         state = st.copy( active = true )
//      }
//   }

   protected def startThinking( implicit tx: ProcTxn ) {
      val st = state
      if( !st.thinking ) state = st.copy( thinking = true )
   }

   protected def stopThinking( implicit tx: ProcTxn ) {
      val st = state
      if( st.thinking ) state = st.copy( thinking = false )
   }

   protected def startPlaying( implicit tx: ProcTxn ) {
      val st = state
      if( !st.playing ) state = st.copy( playing = true )
   }

   protected def stopPlaying( implicit tx: ProcTxn ) {
      val st = state
      if( st.playing ) state = st.copy( playing = false )
   }

   def state( implicit tx: ProcTxn ) = stateRef()
   private def state_=( newState: State )( implicit tx: ProcTxn ) {
      val oldState = stateRef.swap( newState )
      if( oldState != newState ) {
         touch
         val u = updateRef()
         updateRef.set( u.copy( state = newState ))
      }
   }

   protected def emptyUpdate  = Update( proc, State( false ))
   protected def fullUpdate( implicit tx: ProcTxn ) = Update( proc, state )

   def stop( implicit tx: ProcTxn ) {

   }

//   def isActive( implicit tx: ProcTxn ) : Boolean = activeRef()
}
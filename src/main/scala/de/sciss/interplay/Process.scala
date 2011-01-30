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
      p ~> collInt
//      if( fadeTime > 0 ) xfade( fadeTime ) { p.play } else p.play
//      ProcHelper.playNewDiff( p, fadeTime )

      val con = p.control( "amp" )
      val amp = con.v

      lazy val pl: Proc.Listener = new Proc.Listener {
         def updated( u: Proc.Update ) {
            if( u.audioBusesConnected.find( b => (b.sourceVertex == p) && (b.out.name == "out") ).isDefined ) {
                // why we need spawn?? wolkenpumpe doesn't get the new state update otherwise. why?? XXX
               ProcTxn.spawnAtomic { implicit tx =>
                  p.removeListener( pl )
//                  if( fadeTime > 0 ) xfade( fadeTime ) { p.play } else p.play
                  if( fadeTime > 0 ) {
                     con.v = 0
                     p.play
                     glide( fadeTime ) { con.v = amp }
                  } else p.play
//                  postFun( tx )
               }
            }
         }
      }
      p.addListener( pl )
   }

   def canReplaceTail( implicit tx: ProcTxn ) : Boolean = collInt.audioInput( "in" ).edges.nonEmpty

   def replaceTail( p: Proc, fadeTime: Double = 0.0 )( implicit tx: ProcTxn ) {
      val oldIn   = collInt.audioInput( "in" )
      val es      = oldIn.edges
//      if( es.isEmpty ) return false
require( es.nonEmpty )
      val newIn   = p.audioInput( "in" )
      es.foreach { e =>
         e.out ~/> oldIn
         e.out ~> newIn
      }
      if( fadeTime > 0 ) {
         p.bypass
         p ~> collInt
         p.play
         xfade( fadeTime ) { p.engage }
      } else {
         p ~> collInt
         p.play
      }
      true
   }

//   def replaceTailOrDispose( p: Proc, fadeTime: Double = 0.0 )( implicit tx: ProcTxn ) : Boolean = {
//      val res = replaceTail( p, fadeTime )
//      if( !res ) removeAndDispose( p )
//      res
//   }


   def timeString = (new java.util.Date()).toString

   def removeAndDispose( p: Proc, fadeTime: Double = 0.0 )( implicit tx: ProcTxn ) {
//      if( fadeTime > 0 ) xfade( fadeTime ) { p.stop } else p.stop
      if( fadeTime > 0 ) glide( fadeTime ) {
//         println( "old cv " + p.control( "amp" ).cv )
         p.control( "amp" ).v = 0
//         println( "new cv " + p.control( "amp" ).cv )
      }
//if( fadeTime > 0 ) println( "Wowo " + p.hashCode + " - " + p.control( "amp" ).cv.mapping.isEmpty+ " ; target " + p.control( "amp" ).cv.target + "; glidetime " + fadeTime + " ; " + timeString )
//      ProcHelper.whenFadeDone( p ) { implicit tx => }
      ProcHelper.whenGlideDone( p, "amp" ) { implicit tx =>
//         println( "glide done: " + p.hashCode + " : " + timeString )
         def flonky( p: Proc ) {
            p.audioOutputs.flatMap( _.edges ).foreach( e => e.out ~/> e.in )
            val srcs: Set[ Proc ] = p.audioInputs.flatMap( _.edges ).map( e => {
               val pSrc = e.sourceVertex
               if( pSrc.isPlaying ) pSrc.stop
               e.out ~/> e.in
               pSrc
            })( collection.breakOut )
            p.dispose
//println( "  disposed: " + p.name )
            srcs.foreach( flonky( _ ))
         }
//println( "---- begin disposal of " + p.name )
         flonky( p )
//println( "---- end" )
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
         val numFrames  = buf.framesWritten - frameInteg + 1
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

   def searchAnalysisM( frameInteg: Int, maxResults: Int = 20, measure: Array[ Array[ Float ]] => Float )
                      ( fun: ISortedSet[ Sample ] => Unit, rotateBuf: Boolean = false ) {
      require( maxResults > 0, "maxResults must be > 0, but is " + maxResults )
      spawn {
         inform( "searchAnalysisM started" )
         val buf        = anaClientBuf
         val numChannels= buf.numChannels
         val frames     = Array.ofDim[ Float ]( buf.numFrames, numChannels )
         val numFrames  = buf.framesWritten - frameInteg + 1
         var res        = ISortedSet.empty[ Sample ]( sampleOrd )
         var resCnt     = 0
         val frameIntegM= frameInteg - 1

         def karlheinz( idx: Int ) {
            val m = measure( frames )
            if( resCnt < maxResults ) {
               res += Sample( idx, m )
               resCnt += 1
            } else if( res.last.measure > m ) {
               res = res.dropRight( 1 ) + Sample( idx, m )
            }
         }

         if( numFrames > 0 ) {
            var x = 0; while( x < frameInteg ) {
               buf.getFrame( 0, frames( x ))
            x += 1 }
            karlheinz( 0 )
         }
         var off = 1; while( off < numFrames ) {
//            val fm = frameMeasure( buf.getFrame( off, chanBuf ))
            if( rotateBuf ) {
               var y = 0; while( y < numChannels ) {
                  var prev = frames( 0 )( y )
                  var x = frameIntegM; while( x >= 0 ) {   // ouch....
                     val tmp = frames( x )( y )
                     frames( x )( y ) = prev
                     prev = tmp
                  x -= 1 }
               y += 1 }
               buf.getFrame( off, frames( frameIntegM ))
            } else {
               buf.getFrame( off, frames( (off - 1) % frameInteg ))
            }
            karlheinz( off )
         off += 1 }

         inform( "searchAnalysisM done" )
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
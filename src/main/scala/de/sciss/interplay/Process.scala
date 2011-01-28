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

import de.sciss.synth.proc.ProcTxn
import InterPlay._
import SoundProcesses._
import de.sciss.synth.Model
import collection.immutable.{IndexedSeq => IIdxSeq, SortedSet => ISortedSet}
import actors.Actor

object Process {
   val verbose = true
   val all = List( ProcSehen, ProcHoeren, ProcRiechen, ProcSchmecken, ProcTasten, ProcOrientieren, ProcGleichgewichten )

   private val actor = new Actor { def act = loop { react {
      case d: Do => d.perform
   }}}

   def init( implicit tx: ProcTxn ) {
      actor.start
      all.foreach( _.init )
   }

   def secsToFrames( secs: Double ) = (secs * (SAMPLE_RATE / anaWinStep)).toInt
   def framesToPos( idx: Int )      = idx.toDouble / (anaClientBuf.numFrames - 1)

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
}

trait Process {
   def name : String
   def verbose : Boolean

   def init(  implicit tx: ProcTxn ) : Unit
   def start( implicit tx: ProcTxn ) : Unit
   def stop(  implicit tx: ProcTxn ) : Unit

   protected def inform( what: => String ) = if( verbose ) println( name + ": " + what )
}
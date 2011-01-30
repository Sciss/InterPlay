/*
 *  ProcSehen.scala
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

import SoundProcesses._
import de.sciss.synth._
import io.{AudioFile, AudioFileSpec}
import java.io.File
import proc._
import ugen._
import DSL._
import InterPlay._
import Util._
import de.sciss.fscape.FScapeJobs

object ProcSehen extends Process {
   import Process._

   val name    = "p-seh"
   val verbose = false

   val ANA_DUR    = 1.0
   val MIN_WAIT   = 60.0
   val MAX_WAIT   = 120.0

   val LIVE_PROB  = 0.3333
   val INT_PROB   = 0.3333

   def init(  implicit tx: ProcTxn ) {
      filter( name ) {
         graph { in =>
            import AnalysisBuffer._
            val chain      = FFT( bufEmpty( anaFFTSize ).id, Mix( in ), anaFFTOver.reciprocal )
            val coeffs     = MFCC.kr( chain, numMelCoeffs )
            val fftTrig    = Impulse.kr( SampleRate.ir / anaWinStep )
            val fftCnt     = PulseCount.kr( fftTrig )
            val me         = Proc.local
            val anaFrames  = (ANA_DUR * SAMPLE_RATE / anaWinStep + 0.5).toInt
            val anaBuf     = Similarity.Mat( anaFrames, anaChans )
            fftTrig.react( fftCnt +: coeffs.outputs ) { data =>
               val iter    = data.iterator
               val cnt     = iter.next.toInt - 1
               if( cnt < anaFrames ) {
                  val frame = anaBuf.arr( cnt )
                  var i = 0; while( i < numMelCoeffs ) {
                     frame( i ) = (iter.next.toFloat + normAdd( i )) * normMul( i )
                  i += 1 }
               } else {
                  ProcTxn.spawnAtomic { implicit tx =>
//                     me.stop
//                     me.control( "pos" ).v = 1.0
                     ProcHelper.stopAndDispose( me )
                     processAnalysis( anaBuf )
                  }
               }
            }
            in // thru
         }
      }

      start
   }

   def start( implicit tx: ProcTxn ) {
      val waitTime   = rrand( MIN_WAIT, MAX_WAIT )
      inform( "waitForAnalysis " + waitTime )
      startThinking
      waitForAnalysis( waitTime )( analysisReady )
   }

   private def analysisReady {
      ProcTxn.atomic { implicit tx =>
         val p = factory( name ).make
         val pt = if( liveActive ) {
            val rnd = rand( 1.0 )
            if( rnd <= LIVE_PROB ) ReplaceLive else if( rnd - LIVE_PROB <= INT_PROB ) ReplaceInternal else ReplaceAll
         } else ReplaceInternal
         replaceTail( p, point = pt )
      }
   }

   private def processAnalysis( mat: Similarity.Mat ) {
      inform( "processAnalysis" )

      val f       = File.createTempFile( "tmp", ".aif" )
      val spec    = AudioFileSpec( numChannels = 1, sampleRate = anaClientBuf.sampleRate )
      val af      = AudioFile.openWrite( f, spec )
      val afBuf   = af.frameBuffer( 1024 )
      val afChan  = afBuf( 0 )
      var pos     = 0

      def flush {
         af.writeFrames( afBuf, 0, pos )
         pos = 0
      }

      def processMeasure( dstMat: Similarity.Mat ) : Float = {
         val m = Similarity.xcorr( mat )( dstMat )
         afChan( pos ) = m
         pos += 1
         if( pos == 1024 ) flush
         m
      }

      searchAnalysisM( mat.numFrames,
                       maxResults = 1, // hmmm...
                       measure = processMeasure( _ )) { _ =>

         flush
         af.close
         inform( "ready for murke" )
//         val doc = FScapeJobs.Murke( inPath.getAbsolutePath, outPath.getAbsolutePath, OutputSpec.aiffInt, Gain.normalized, filter = "daub16", trunc = true )
//         fsc.process( "wavelet", doc ) {
//            case true   => ProcTxn.spawnAtomic { implicit tx => inject( outPath )}
//            case false  => println( "Failure!" )
//         }
//         FScape.injectWavelet
//         ProcTxn.spawnAtomic { implicit tx =>
//
//         }
      }
   }
}
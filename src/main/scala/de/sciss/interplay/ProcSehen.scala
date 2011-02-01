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

   val name          = "p-seh"
   val verbose       = true

   val ANA_DUR       = 1.0
   val MIN_WAIT      = 60.0
   val MAX_WAIT      = 120.0
//val MIN_WAIT      = 30.0
//val MAX_WAIT      = 30.0

   val LIVE_PROB     = 0.3333
   val INT_PROB      = 0.3333

   val MIN_THRESH    = 1.0    // powers of mean, thus 1 = mean, 2 = mean.pow( 2 ), etc.
   val MAX_THRESH    = 0.5    // powers of mean
   val MIN_HYST      = 0.8    // hysteresis as factor of up-thresh
   val MAX_HYST      = 0.6    // hysteresis as factor of up-thresh

   val MIN_SPEED     = 0.5
   val MAX_SPEED     = 1.0

   val MIN_REENTRY   = 60.0
   val MAX_REENTRY   = 90.0

   private val orgRef = Ref( Map.empty[ Proc, Org ])
   private case class Org( gen: Proc, diff: Proc, path: String )

   private lazy val anaName = name + "-ana"

   def init(  implicit tx: ProcTxn ) {
      filter( anaName ) {
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
                     ProcessHelper.stopAndDispose( me )
                     processAnalysis( anaBuf )
                  }
               }
            }
            in // thru
         }
      }

      gen( name ) {
         val pframes = pScalar( "frames", ParamSpec( 1, maxLiveFrames ), SAMPLE_RATE )
         val pspeed  = pAudio(  "speed",  ParamSpec( 1.0/8, 8.0, ExpWarp ), 1.0 )
         graph {
            val me         = Proc.local
            val org        = ProcTxn.atomic( orgRef()( _ ))( me )
            val spec       = audioFileSpec( org.path )
            val speed      = pspeed.ar
            val sig        = VDiskIn.ar( spec.numChannels, bufCue( org.path ).id, speed )
            val frameInteg = Integrator.ar( speed )
            val done       = frameInteg > (pframes.ir + SAMPLE_RATE)
            done.react {
               ProcTxn.spawnAtomic { implicit tx =>
                  orgRef.transform( _ - me )
                  Process.removeAndDispose( org.diff, 0.1 )
               }
            }
            sig
         }
      }

      start
   }

   private def start( implicit tx: ProcTxn ) {
      val waitTime   = rrand( MIN_WAIT, MAX_WAIT )
      inform( "waitForAnalysis " + waitTime )
      startThinking
      waitForAnalysis( waitTime )( analysisReady )
   }

   private def reentry( implicit tx: ProcTxn ) {
      val dlyTime = rrand( MIN_REENTRY, MAX_REENTRY )
      delay( dlyTime )( ProcTxn.atomic( start( _ )))
   }

   private def analysisReady {
      ProcTxn.spawnAtomic { implicit tx =>  // XXX must spawn, don't know why? otherwise system blows up!
         inform( "analysisReady" )
         startThinking
         val pt = if( liveActive ) {
            val rnd = rand( 1.0 )
            if( rnd <= LIVE_PROB ) ReplaceLive else if( rnd - LIVE_PROB <= INT_PROB ) ReplaceInternal else ReplaceAll
         } else ReplaceInternal
         if( canReplaceTail( pt )) {
            val p = factory( anaName ).make
            replaceTail( p, point = pt )
         }
      }
   }

   private def processAnalysis( mat: Similarity.Mat )( implicit tx: ProcTxn ) {
      Process.afterCommit( tx )( actProcessAna( mat ))
   }

   private def actProcessAna( mat: Similarity.Mat ) {
      val f             = File.createTempFile( "tmp", ".aif" )
      val spec          = AudioFileSpec( numChannels = 1, sampleRate = anaClientBuf.sampleRate )
      val afCtrl        = AudioFile.openWrite( f, spec )
      val afBuf         = afCtrl.frameBuffer( 1024 )
      val afChan        = afBuf( 0 )
      var pos           = 0
      val numAnaFrames  = availableLiveRecordingFrames
      informDir( "processAnalysis " + numAnaFrames )

      def flush {
         afCtrl.writeFrames( afBuf, 0, pos )
         pos = 0
      }

      var sum          = 0.0
      def processMeasure( dstMat: Similarity.Mat ) : Float = {
         val m = Similarity.xcorr( mat )( dstMat )
         if( pos < numAnaFrames ) {
            afChan( pos ) = m
            pos += 1
            if( pos == 1024 ) flush
         }
         sum += m
         m
      }

      def truncDone( res: Option[ String ]) = res match {
         case Some( inPath ) =>
            informDir( "ready for murke" )
            val mean       = sum / numAnaFrames
            val upThresh   = rrand( MIN_THRESH, MAX_THRESH ) * mean
            val downThresh = upThresh * rrand( MIN_HYST, MAX_HYST )
            val ctrlPath   = afCtrl.file.get.getAbsolutePath()
            val outPath    = File.createTempFile( "fsc", ".aif" ).getAbsolutePath()
            val doc = FScapeJobs.DrMurke(
               inPath, ctrlPath, outPath, FScapeJobs.OutputSpec.aiffInt, FScapeJobs.Gain.normalized,
               mode = "up", threshUp = upThresh.toString, threshDown = downThresh.toString,
               durUp = "0.1s", durDown = "0.1s", attack = "0.01s", release = "1.0s", spacing = Some( "0s" ))
            FScape.fsc.process( "murke", doc ) {
               case true   => ProcTxn.atomic { implicit tx =>
                  stopThinking
                  startPlaying
                  inject( outPath )
                  reentry
               }
               case false  => informDir( "FScape failure!", force = true )
            }
         case None =>
            informDir( "Wooop. Something went wrong. No truncated live file", force = true )
            ProcTxn.atomic { implicit tx => stopThinking }
      }

      def measureDone {
         flush
         afCtrl.close
         informDir( "getting trunc file" )
         ProcTxn.atomic( implicit tx => truncateLiveRecording( numAnaFrames )( truncDone( _ )))
      }

      // grmpfff
      ProcTxn.atomic( implicit tx => searchAnalysisM( mat.numFrames,
                       maxResults = 1, // hmmm...
                       measure = processMeasure( _ ))( _ => measureDone ))
   }

   private def inject( path: String )( implicit tx: ProcTxn ) {
      val spec = audioFileSpec( path )
      val d = factory( "O-all" ).make
      val g = factory( name ).make
      val org  = Org( g, d, path )
      orgRef.transform( _ + (g -> org) )
      g.control( "frames" ).v = spec.numFrames
      g.control( "speed" ).v  = rrand( MIN_SPEED, MAX_SPEED )
      g ~> d
      Process.addTail( d, 0.1 )
   }
}
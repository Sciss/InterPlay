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
import Tendency._

/**
 * Picks up the current spectral and temporal pattern from the sum signal,
 * and measures the similarity in the live buffer. Uses FScape's DrMurke
 * to sort out similar sounds which are the re-injected.
 */
object ProcSehen extends Process {
   import Process._

   val name          = "p-seh"
   val verbose       = true

//   val ANA_DUR       = 1.0
   val TEND_ANA_DUR  = tend( name + "ana_dur", Exp, 0.0 -> (1.0, 1.0), 0.9 -> (0.9, 1.1), 2.0 -> (0.5, 1.0) )

   val MIN_WAIT      = 60.0
   val MAX_WAIT      = 120.0
//val MIN_WAIT      = 30.0
//val MAX_WAIT      = 30.0

//   val LIVE_PROB     = 0.3333
//   val INT_PROB      = 0.3333
   val TEND_LIVE_PROB  = tend( name + "-live_prob", Lin, 0.0 -> (0.6667, 0.6667), 1.0 -> (0.3333, 0.3333) )
   val TEND_INT_PROB   = tend( name + "-int_prob",  Lin, 0.0 -> (0.1667, 0.1667), 1.0 -> (0.3333, 0.3333) )

//   val MIN_THRESH    = 1.0    // powers of mean, thus 1 = mean, 2 = mean.pow( 2 ), etc.
//   val MAX_THRESH    = 0.5    // powers of mean
   val TEND_THRESH   = tend( name + "-thresh", Lin, 0.0 -> (0.5, 1.0), 0.9 -> (0.5, 1.0), 2.0 -> (0.5, 3.0) )

//   val MIN_HYST      = 0.8    // hysteresis as factor of up-thresh
//   val MAX_HYST      = 0.6    // hysteresis as factor of up-thresh
   val TEND_HYST     = tend( name + "-hyst", Lin, 0.0 -> (0.6, 0.8), (2.0, (0.6, 0.95), 'sin) )

//   val MIN_SPEED     = 0.5
//   val MAX_SPEED     = 1.0
   val TEND_SPEED    = tend( name + "-speed", Exp, 0.0 -> (1.0, 1.0), 1.0 -> (0.5, 1.0), 2.0 -> (0.25, 1.0) )

//   val MIN_REENTRY   = 60.0
//   val MAX_REENTRY   = 90.0
   val TEND_REENTRY  = tend( name + "-reentry", Lin, 0.0 -> (60.0, 90.0), 1.0 -> (60.0, 90.0), 2.0 -> (10.0, 70.0) )

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
            val anaFrames  = (TEND_ANA_DUR.decide * SAMPLE_RATE / anaWinStep + 0.5).toInt
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
      val dlyTime = TEND_REENTRY.decide
      inform( "reentry after " + dlyTime + "s" )
      delay( dlyTime )( ProcTxn.atomic( start( _ )))
   }

   private def analysisReady {
      ProcTxn.spawnAtomic { implicit tx =>  // XXX must spawn, don't know why? otherwise system blows up!
         val pt = if( liveActive ) {
            val rnd = rand( 1.0 )
            val liveProb   = TEND_LIVE_PROB.decide
            val intProb    = TEND_INT_PROB.decide
            if( rnd <= liveProb ) ReplaceLive else if( rnd - liveProb <= intProb ) ReplaceInternal else ReplaceAll
         } else ReplaceInternal
         val ok = canReplaceTail( pt )
         inform( "analysisReady " + ok )
         if( ok ) {
            startThinking
            val p = factory( anaName ).make
            replaceTail( p, point = pt )
//         } else {
//            fastReentry
         }

         reentry
      }
   }

   private def processAnalysis( mat: Similarity.Mat )( implicit tx: ProcTxn ) {
      Process.afterCommit( tx )( actProcessAna( mat ))
   }

   private def actProcessAna( mat: Similarity.Mat ) {
      try {
         val f             = File.createTempFile( "tmp", ".aif" )
         val spec          = AudioFileSpec( numChannels = 1, sampleRate = anaClientBuf.sampleRate )
         val afCtrl        = AudioFile.openWrite( f, spec )
         val afBuf         = afCtrl.frameBuffer( 1024 )
         val afChan        = afBuf( 0 )
         var pos           = 0
         val numAnaFrames  = availableLiveRecordingFrames
         informDir( "processAnalysis " + numAnaFrames )
         if( numAnaFrames == 0 ) return

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
               val upThresh   = TEND_THRESH.decide * mean
               val downThresh = upThresh * TEND_HYST.decide
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
//                     reentry
                  }
                  case false => informDir( "FScape failure!", force = true )
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
      } catch {
         case e =>
            informDir( "Error in process-analysis:", force = true )
            e.printStackTrace()
//            ProcTxn.atomic( fastReentry( _ ))
      }
   }

   private def inject( path: String )( implicit tx: ProcTxn ) {
      val spec = audioFileSpec( path )
      val d = factory( "O-all" ).make       // XXX should use different spats
      val g = factory( name ).make
      val org  = Org( g, d, path )
      orgRef.transform( _ + (g -> org) )
      g.control( "frames" ).v = spec.numFrames
      g.control( "speed" ).v  = TEND_SPEED.decide
      g ~> d
      Process.addTail( d, 0.1 )
   }
}
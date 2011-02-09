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
   val TEND_THRESH   = tend( name + "-thresh", Lin, 0.0 -> (0.5, 1.0), 1.4 -> (0.5, 1.0), 2.0 -> (0.5, 3.0) )

//   val MIN_HYST      = 0.8    // hysteresis as factor of up-thresh
//   val MAX_HYST      = 0.6    // hysteresis as factor of up-thresh
   val TEND_HYST     = tend( name + "-hyst", Lin, 0.0 -> (0.6, 0.8), 1.5 -> (0.6, 0.8), (2.0, (0.6, 0.95), 'sin) )

//   val MIN_SPEED     = 0.5
//   val MAX_SPEED     = 1.0
   val TEND_SPEED    = tend( name + "-speed", Exp, 0.0 -> (1.0, 1.0), 1.0 -> (0.5, 1.0), 2.0 -> (0.25, 1.0) )

//   val MIN_REENTRY   = 60.0
//   val MAX_REENTRY   = 90.0
   val TEND_REENTRY  = tend( name + "-reentry", Lin, 0.0 -> (60.0, 90.0), 1.0 -> (60.0, 90.0), 1.6 -> (20.0, 74.0), 1.61 -> (9999.0, 9999.0) )

   val MAX_TIME        = 1.7

   val MIN_ERASE     = 20.0
   val MAX_ERASE     = 30.0

   val MAX_PIPE      = 2

   private val orgRef = Ref( Map.empty[ Proc, Org ])
   private case class Org( gen: Proc, diff: Proc, path: String )
   private val eraseRef = Ref( Map.empty[ Proc, Proc ])  // erase to gen

   private lazy val anaNameD  = name + "-anad"
   private lazy val anaNameF  = name + "-anaf"
   private lazy val eraseName = name + "-erase"

   private val inThePipeline = Ref( 0 )

   def init(  implicit tx: ProcTxn ) {
      def flonky( in: GE ) {
         import AnalysisBuffer._
         val chain      = FFT( bufEmpty( anaFFTSize ).id, Mix( in ), anaFFTOver.reciprocal )
         val coeffs     = MFCC.kr( chain, numMelCoeffs )
         val fftTrig    = Impulse.kr( SampleRate.ir / anaWinStep ) & (Mix( coeffs ) > 0)
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
               spawnAtomic( name + "ana removal" ) { implicit tx =>
//                     me.stop
//                     me.control( "pos" ).v = 1.0
                  ProcessHelper.stopAndDispose( me )
                  processAnalysis( anaBuf )
               }
            }
         }
      }

      diff( anaNameD ) {
         graph { in =>
            flonky( in )
            0.0
         }
      }

      filter( anaNameF ) {
         graph { in =>
            flonky( in )
            in // thru
         }
      }

      gen( name ) {
         val pframes = pScalar( "frames", ParamSpec( 1, maxLiveFrames ), SAMPLE_RATE )
         val pspeed  = pAudio(  "speed",  ParamSpec( 1.0/8, 8.0, ExpWarp ), 1.0 )
         graph {
            val me         = Proc.local
            val org        = atomic( name + " gen getorg" )( orgRef()( _ ))( me )
            val spec       = audioFileSpec( org.path )
            val speed      = pspeed.ar
            val sig        = VDiskIn.ar( spec.numChannels, bufCue( org.path ).id, speed )
            val frameInteg = Integrator.ar( speed )
            val done       = frameInteg > (pframes.ir + SAMPLE_RATE)
            done.react {
               spawnAtomic( name + " gen removal" ) { implicit tx =>
                  val map = orgRef()
                  map.get( me ).foreach { org =>
                     orgRef.set( map - me )
                     Process.removeAndDispose( name + " gen removal", org.diff, 0.1 )
                  }
               }
            }
            sig
         }
      }

      filter( eraseName ) {
         val pdur = pScalar( "dur", ParamSpec( 2.0, 60.0, ExpWarp ), 10.0 )
         graph { in =>
            val me      = Proc.local
            val dur     = pdur.ir
            val freq    = XLine.ar( 20, 18000, dur - 1 )
            val fadeIn  = Line.ar( -1, 1, 1 )
            val fadeOut = EnvGen.ar( Env.asr( 0, 1, 1 ), gate = 1 - Done.kr( freq ))
            val flt     = HPF.ar( in, freq ) * fadeOut
            Done.kr( fadeOut ).react {
               spawnAtomic( name + " gen removal" ) { implicit tx =>
                  val map = eraseRef()
                  map.get( me ).foreach { pref =>
                     orgRef.transform( _ - pref )
                     Process.removeAndDispose( name + " erase removal", me )
                  }
               }
            }
            LinXFade2.ar( in, flt, fadeIn )
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

   private def reentry( factor: Double )( implicit tx: ProcTxn ) {
      val dlyTime = TEND_REENTRY.decide * factor
      inform( "reentry after " + dlyTime + "s" )
      delay( dlyTime )( spawnAtomic( name + " reentry done" )( start( _ )))
   }

   override protected def stopped( implicit tx: ProcTxn ) {
      orgRef().values.map { org =>
         org.diff.audioOutput( "out" ).edges.map( _.targetVertex ).find( v => !v.name.startsWith( "$" )).foreach { ptgt =>
            val perase = factory( eraseName ).make
            perase.control( "dur" ).v = exprand( MIN_ERASE, MAX_ERASE )
            org.diff ~| perase |> ptgt
            if( !perase.isPlaying ) perase.play
            eraseRef.transform( _ + (perase -> org.gen) )
         }
      }
   }

   private def analysisReady {
      spawnAtomic( name + " analysisReady" ) { implicit tx =>  // XXX must spawn, don't know why? otherwise system blows up!
         if( keepGoing && (SoundProcesses.logicalTime < MAX_TIME) ) {
            val ok = if( inThePipeline() < MAX_PIPE ) {
               if( liveActive ) {
                  val rnd = rand( 1.0 )
                  val liveProb   = TEND_LIVE_PROB.decide
                  val intProb    = TEND_INT_PROB.decide
                  val pt         = if( rnd <= liveProb ) ReplaceLive else if( rnd - liveProb <= intProb ) ReplaceInternal else ReplaceAll
                  val res        = canReplaceTail( pt )
                  if( res ) {
                     val p = factory( anaNameF ).make
                     replaceTail( p, point = pt )
                  }
                  res
               } else {
                  val p = factory( anaNameD ).make
                  collAll ~> p
                  p.play
                  true
               }
            } else false

            inform( "analysisReady " + ok )
            if( ok ) startThinking

            reentry( if( ok ) 1.0 else 0.1 )
         }
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
               atomic( name + " add to pipeline" ) { implicit tx => inThePipeline.transform( _ + 1 )}
               val doc = FScapeJobs.DrMurke(
                  inPath, ctrlPath, outPath, FScapeJobs.OutputSpec.aiffInt, FScapeJobs.Gain.normalized,
                  mode = "up", threshUp = upThresh.toString, threshDown = downThresh.toString,
                  durUp = "0.1s", durDown = "0.1s", attack = "0.01s", release = "1.0s", spacing = Some( "0s" ))
               FScape.fsc.process( "murke", doc ) { success => // fsc2
                  informDir( "murke done " + success )
                  // atomic can lead to timeout here...
                  spawnAtomic( name + " fscape done " + success ) { implicit tx =>
                     if( success ) {
                        stopThinking
                        startPlaying
                        inject( outPath )
                     } else {
                        inform( "FScape failure!", force = true )
                     }
                     inThePipeline.transform( _ - 1 )
                  }
               }
            case None =>
               informDir( "Wooop. Something went wrong. No truncated live file", force = true )
               atomic( name + " trunc failed" ) { implicit tx => stopThinking }
         }

         def measureDone {
            flush
            afCtrl.close
            informDir( "getting trunc file" )
            atomic( name + " measure done" )( implicit tx => truncateLiveRecording( numAnaFrames )( truncDone( _ )))
         }

         // grmpfff
         atomic( name + " start searchAnalysisM" )( implicit tx => searchAnalysisM( mat.numFrames,
                          maxResults = 1, // hmmm...
                          measure = processMeasure( _ ))( _ => measureDone ))
      } catch {
         case e =>
            informDir( "Error in process-analysis:", force = true )
            e.printStackTrace()
//            atomic( fastReentry( _ ))
      }
   }

   private def inject( path: String )( implicit tx: ProcTxn ) {
      inform( "inject; pipe = " + inThePipeline() )
      val spec = audioFileSpec( path )
//      val d = factory( "O-all" ).make       // XXX should use different spats
      val d = factory( "O-pan" ).make
      val spread = rrand( 0.25, 1.0 )
      d.control( "spr" ).v    = spread    // 0 to 1
      d.control( "rota" ).v   = 1   // amt 0 to 1
      d.control( "azi" ).v    = rand( 360.0 )    // 0 to 360
      d.control( "speed" ).v  = exprand( 1.0 / 20, 1.0 / 10 )  // freq
      d.control( "amp" ).v    = 1.0 / math.sqrt( spread )   // account for less energy with lower spread?

      val g = factory( name ).make
      val org  = Org( g, d, path )
      orgRef.transform( _ + (g -> org) )
      g.control( "frames" ).v = spec.numFrames
      g.control( "speed" ).v  = TEND_SPEED.decide
      g ~> d
      Process.addTail( d, 0.1 )
   }
}
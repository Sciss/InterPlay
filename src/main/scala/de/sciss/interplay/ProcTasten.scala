/*
 *  ProcTasten.scala
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
import Util._
import io.AudioFile
import java.io.File
import de.sciss.fscape.FScapeJobs
import collection.breakOut

/**
 * Uses predefined templates to match the live buffer against. Isolates little
 * gestures of similarity to the templates and plays them back as one-shots.
 */
object ProcTasten extends Process {
   import Process._

   val name    = "p-tast"
   val verbose = true

   val MIN_WAIT      = 60.0
   val MAX_WAIT      = 120.0
//   val INIT_THRESH   = 0.5f

   val MIN_REENTRY   = 60.0
   val MAX_REENTRY   = 90.0

   private val orgRef = Ref( Map.empty[ Proc, Org ])
   private case class Org( gen: Proc, diff: Proc, path: String )

   def init(  implicit tx: ProcTxn ) {
      gen( name ) {
         val pdur = pScalar( "dur", ParamSpec( 1, 240 ), 20 )
         graph {
            val me   = Proc.local
            val org  = ProcTxn.atomic( orgRef()( _ ))( me )
            val spec = audioFileSpec( org.path )
            val sig  = DiskIn.ar( spec.numChannels, bufCue( org.path ).id )
            Done.kr( Line.kr( 0, 0, pdur.ir )).react {
               ProcTxn.spawnAtomic { implicit tx =>
                  orgRef.transform( _ - me )
//                  ProcessHelper.stopAndDispose( d, 0.1, postFun = tx => ProcessHelper.stopAndDispose( g )( tx ))
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
      waitForAnalysis( waitTime )( waitForAnaDone )
   }

   private def waitForAnaDone {
      val temp = choose( Similarity.templates )._2
      ProcTxn.atomic { implicit tx =>
         playPath match {
            case Some( inPath ) => perform( temp, inPath )
            case None => {
               stopThinking
            }
         }
      }
   }

   def perform( temp: Similarity.Template, inPath: File )( implicit tx: ProcTxn ) {
      searchAnalysisM( frameInteg = temp.mat.numFrames,
                       maxResults = 20,
                       measure = Similarity.xcorr( temp.mat )( _ ))( searchAnaDone( inPath, _ ))
   }

   private def searchAnaDone( inPath: File, res: Iterable[ Sample ]) {
      informDir( "search result : " + res )
      if( res.nonEmpty ) {
         process( inPath, res )
      } else ProcTxn.atomic { implicit tx => stopThinking }
   }

   private def process( inPath: File, res: Iterable[ Sample ]) {
      val inF = try {
         AudioFile.openRead( inPath )
      } catch {
         case e =>
            informDir( "Could not open audiofile for reading: " + inPath, force = true )
            return
      }
      val buf           = inF.frameBuffer( 8192 )

      val jobs: List[ FScapeJobs.Bleach ] = res.map( smp => {
         val (start, stop) = isolateHit( smp.idx )
         val rvsLen        = math.min( 44100L, stop - start )
//         val inF           = AudioFile.openRead( inPath )
         val outF          = FScape.createTempAudioFile( inF )

         // reverse
         var remain = rvsLen
         while( remain > 0 ) {
            val chunkLen = math.min( remain, 8192 ).toInt
            inF.seekFrame( start + remain - chunkLen )
            inF.readFrames( buf, 0, chunkLen )
            DSP.reverse( buf, 0, chunkLen )
            outF.writeFrames( buf, 0, chunkLen )
            remain -= chunkLen
         }

         // rest
         inF.seekFrame( start )
         remain = stop - start
         while( remain > 0 ) {
            val chunkLen = math.min( remain, 8192 ).toInt
            inF.readFrames( buf, 0, chunkLen )
            outF.writeFrames( buf, 0, chunkLen )
            remain -= chunkLen
         }

         outF.close

         val outPath = File.createTempFile( "tmp", ".aif" )

         FScapeJobs.Bleach(
            in       = outF.file.get.getAbsolutePath,
            out      = outPath.getAbsolutePath,
            spec     = FScapeJobs.OutputSpec.aiffFloat,
            gain     = FScapeJobs.Gain.immediate,
            length   = 256,
            feedback = "-50dB",
            clip     = "18dB"
         )
      })( breakOut )

      inF.close

      FScape.fsc.processChain( name, jobs ) { res =>
         if( res ) {
            def gugu( jobs: List[ FScapeJobs.Bleach ], first: Boolean ) {
               jobs match {
                  case bleach :: tail =>
                     ProcTxn.atomic { implicit tx =>
                        if( first ) {
                           stopThinking
                           startPlaying
                           reentry
                        }
//                        FScape.inject( new File( bleach.out ), "O-one" )
                        inject( bleach.out )
                        delay( exprand( 0.2, 1.5 ))( gugu( tail, false ))
                     }
                  case _ =>
               }
            }
            gugu( jobs, true )
         } else {
            ProcTxn.atomic { implicit tx => stopThinking }
            informDir( "FScape failure!", force = true )
         }
      }
   }

   private def reentry( implicit tx: ProcTxn ) {
      val dlyTime = rrand( MIN_REENTRY, MAX_REENTRY )
      delay( dlyTime )( ProcTxn.atomic( start( _ )))
   }

   private def inject( path: String )( implicit tx: ProcTxn ) {
//      cnt += 1
      val spec = audioFileSpec( path ) // AudioFile.readSpec( path )
      val d = factory( "O-one" ).make
      val g = factory( name ).make
      val org  = Org( g, d, path )
      orgRef.transform( _ + (g -> org) )
      val dch = d.control( "idx" )
      dch.v = dch.spec.map( Util.rand( 1.0 ))
      g.control( "dur" ).v = spec.numFrames / spec.sampleRate
      g ~> d
      Process.addTail( d, 0.1 )
   }

   private def isolateHit( idx: Int ) : (Long, Long) = {
      val ws = AnalysisBuffer.anaWinStep
      (idx * ws, (idx + 86) * ws) // XXX TODO
   }
}
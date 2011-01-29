/*
 *  Tasten.scala
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
import SoundProcesses._
import Util._
import de.sciss.synth.io.AudioFile
import java.io.File
import de.sciss.interplay.Similarity.Mat
import de.sciss.fscape.FScapeJobs
import collection.breakOut

object ProcTasten extends Process {
   import Process._

   val name    = "p-tast"
   val verbose = true

   val MIN_WAIT      = 60.0
   val MAX_WAIT      = 120.0
//   val INIT_THRESH   = 0.5f

   def init(  implicit tx: ProcTxn ) {
      val waitTime   = rrand( MIN_WAIT, MAX_WAIT )
      inform( "waitForAnalysis " + waitTime )
      startThinking
      waitForAnalysis( waitTime ) {
         val temp = choose( Similarity.templates )._2
         playPath match {
            case Some( inPath ) => perform( temp, inPath )
            case None => {
               ProcTxn.atomic { implicit tx => stopThinking }
            }
         }
      }
   }

   def perform( temp: Similarity.Template, inPath: File ) {
      searchAnalysisM( frameInteg = temp.mat.numFrames,
                       maxResults = 20,
                       measure = xcorr( temp.mat )( _ )) { res =>
         if( verbose ) inform( "search result : " + res )
         if( res.nonEmpty ) {
            process( inPath, res )
         } else ProcTxn.atomic { implicit tx => stopThinking }
      }
   }

   private def process( inPath: File, res: Iterable[ Sample ]) {
      val jobs: List[ FScapeJobs.Bleach ] = res.map( smp => {
         val (start, stop) = isolateHit( smp.idx )
         val rvsLen        = math.min( 44100L, stop - start )
         val inF           = AudioFile.openRead( inPath )
         val buf           = inF.frameBuffer( 8192 )
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

         inF.close
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
      FScape.fsc.processChain( name, jobs ) { res =>
         if( res ) {
            def gugu( jobs: List[ FScapeJobs.Bleach ], first: Boolean ) {
               jobs match {
                  case bleach :: tail =>
                     ProcTxn.atomic { implicit tx =>
                        if( first ) {
                           stopThinking
                           startPlaying
                        }
                        FScape.inject( new File( bleach.out ), "O-one" )
                     }
                     delay( exprand( 0.2, 1.5 ))( gugu( tail, false ))
                  case _ =>
               }
            }
            gugu( jobs, true )
         } else {
            ProcTxn.atomic { implicit tx => stopThinking }
            println( name + ": FScape failed!" )
         }
      }
   }

   private def xcorr( a: Mat )( b: Array[ Array[ Float ]]) : Float = {
      var sum = 0.0
      var x = 0; while( x < a.numFrames ) {
         val af = a.arr( x )
         val df = b( x )
         var y = 0; while( y < a.numChannels ) {
            sum += af( y ) * df( y )
         y += 1 }
      x += 1 }
      (sum / (a.size - 1)).toFloat
   }

   private def isolateHit( idx: Int ) : (Long, Long) = {
      (idx * anaWinStep, (idx + 86) * anaWinStep) // XXX TODO
   }
}
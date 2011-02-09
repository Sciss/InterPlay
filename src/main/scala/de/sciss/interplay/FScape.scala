/*
 *  FScape.scala
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

import java.io.File
import de.sciss.fscape.FScapeJobs
import InterPlay._
import SoundProcesses._
import de.sciss.synth
import synth.io.{SampleFormat, AudioFile, AudioFileSpec}
import de.sciss.interplay.{Process => IPProcess}
import synth.proc.{Ref, ParamSpec, ProcDemiurg, Proc, ProcTxn, DSL}

object FScape {
   import FScapeJobs._

   val verbose = false

   lazy val fsc = {
      val res = FScapeJobs( numThreads = 3 )
      res.verbose = verbose
//      res.dumpOSC( true )
      res.connect( timeOut = 10.0 ) {
         case true =>
            println( "Connected to FScape." )
         case false =>
            println( "!!!!!!!! FSCAPE CONNECT FAILED !!!!!!!!")
      }
      res
   }

//   lazy val fsc2 = {
//      val res = FScapeJobs()
//      res.verbose = verbose
//      res
//   }
//
//   lazy val fsc3 = {
//      val res = FScapeJobs()
//      res.verbose = verbose
//      res
//   }

   def injectWavelet( inPath: File, amp: Double = 1.0 )( implicit tx: ProcTxn ) {
      IPProcess.afterCommit( tx )( actInjectWavelet( inPath, amp ))
   }

   private def actInjectWavelet( inPath: File, amp: Double ) {
      val outPath = File.createTempFile( "fsc", ".aif" ) // , FSC_PATH
      val doc = Wavelet( inPath.getAbsolutePath, outPath.getAbsolutePath, OutputSpec.aiffInt, Gain.normalized, filter = "daub16", trunc = true )
      fsc.process( "wavelet", doc ) {
         case true   => IPProcess.spawnAtomic( "injectWavelet fscape done" ) { implicit tx => inject( outPath, amp = amp )}
         case false  => println( "Failure!" )
      }
   }

   private val cntRef = Ref( 0 )

   def inject( outPath: File, diffName: String = "O-all", amp: Double = 1.0 )( implicit tx: ProcTxn ) {
      import DSL._
      import synth._
      import ugen._

      cntRef.transform( _ + 1 )
      val cnt = cntRef()
      val spec = AudioFile.readSpec( outPath )
      val d = factory( diffName ).make
      lazy val g: Proc = (gen( "fsc" + cnt ) {
         val pdur = pScalar( "dur", ParamSpec( 1, 240 ), 20 )
         graph {
            val sig = DiskIn.ar( spec.numChannels, bufCue( outPath.getAbsolutePath ).id )
            Done.kr( Line.kr( 0, 0, pdur.ir )).react {
               IPProcess.spawnAtomic( "fscape inject removal" ) { implicit tx =>
//                  ProcessHelper.stopAndDispose( d, 0.1, postFun = tx => ProcessHelper.stopAndDispose( g )( tx ))
                  IPProcess.removeAndDispose( "fscape inject removal", d, 0.1 )
               }
            }
            sig
         }
      }).make

      if( diffName == "O-one" ) {
         val dch = d.control( "idx" )
         dch.v = dch.spec.map( Util.rand( 1.0 ))
      }
      g.control( "dur" ).v = spec.numFrames / spec.sampleRate
      if( amp != 1.0 ) d.control( "amp" ).v = amp
      g ~> d
//      ProcessHelper.playNewDiff( 0.1, d )
      IPProcess.addTail( d, 0.1 )

      // another spawn necessary because sucky wolkenpumpe puts
      // the output bus to meter connection later
      // ; XXX TODO : this is still a race condition; we should thus
      // register a model and wait for that bastard to be ready...
//      spawnAtomic { implicit tx => xfade( 0.1 ) { d.play }}
   }

   def createTempAudioFile( src: AudioFile, sampleFormat: Option[ SampleFormat ] = None ) : AudioFile = {
      val f    = File.createTempFile( "tmp", ".aif" )
      val spec = AudioFileSpec( numChannels = src.numChannels, sampleRate = src.sampleRate,
         sampleFormat = sampleFormat.getOrElse( src.sampleFormat ))
      AudioFile.openWrite( f, spec )
   }
}
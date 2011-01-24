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
import synth.io.AudioFile
import synth.proc.{ParamSpec, ProcDemiurg, Proc, ProcTxn, DSL}

object FScape {
   import FScapeJobs._

   lazy val fsc = {
      val res = FScapeJobs()
      res.verbose = true
      res
   }

   def injectWavelet( inPath: File ) {
      val outPath = File.createTempFile( "fsc", ".aif", new File( REC_PATH, "fsc" ))
      val doc = Wavelet( inPath.getAbsolutePath, outPath.getAbsolutePath, OutputSpec.aiffInt, Gain.normalized, filter = "daub16", trunc = true )
      fsc.process( "wavelet", doc ) {
         case true   => ProcTxn.spawnAtomic { implicit tx => inject( outPath )}
         case false  => println( "Failure!" )
      }
   }

   private var cnt = 0

   private def inject( outPath: File )( implicit tx: ProcTxn ) {
      import DSL._
      import synth._
      import ugen._

      cnt += 1
      val spec = AudioFile.readSpec( outPath )
      val d = ProcDemiurg.factories( tx ).find( _.name == "O-one" ).get.make
      lazy val g: Proc = (gen( "fsc" + cnt ) {
         val pdur = pScalar( "dur", ParamSpec( 1, 240 ), 20 )
         graph {
            val sig = DiskIn.ar( spec.numChannels, bufCue( outPath.getAbsolutePath ).id )
            Done.kr( Line.kr( 0, 0, pdur.ir )).react {
               ProcTxn.spawnAtomic { implicit tx =>
                  ProcHelper.stopAndDispose( 0.1, d, postFun = tx => ProcHelper.stopAndDispose( 0.0, g )( tx ))
               }
            }
            sig
         }
      }).make

      g.control( "dur" ).v = spec.numFrames / spec.sampleRate
      g ~> d
      // another spawn necessary because sucky wolkenpumpe puts
      // the output bus to meter connection later
      // ; XXX TODO : this is still a race condition; we should thus
      // register a model and wait for that bastard to be ready...
      ProcTxn.spawnAtomic { implicit tx => xfade( 0.1 ) { d.play }}
   }
}
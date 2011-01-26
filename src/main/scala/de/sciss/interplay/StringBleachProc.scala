/*
 *  StringBleachProc.scala
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

import actors.Actor
import de.sciss.interplay.Similarity.Template
import SoundProcesses._
import de.sciss.synth.io.AudioFile
import java.io.File
import de.sciss.fscape.FScapeJobs
import FScapeJobs._
import de.sciss.synth.proc.ProcTxn

object SearchBleachProc {
   private case class Perform( temp: Template, inPath: File )
}
case class SearchBleachProc( name: String, templateName: String ) {
   import SearchBleachProc._

//   val name = "StringBleach"
   val verbose = true

   private lazy val actor = (new Actor { def act = loop { react {
      case p: Perform => performAct( p )
      case x => println( name + ": Unknown message " + x )
   }}}).start

   def perform { (Similarity.templates.get( templateName ), playPath) match {
      case (Some( temp ), Some( path )) => actor ! Perform( temp, path )
      case (None, _) => println( name + ": No template named '" + templateName + "'" )
      case (_, None) => println( name + ": No play path" )
   }}

   private def performAct( p: Perform ) {
      if( verbose ) println( name + ": act : " + p )
      Similarity.search( p.temp, 0.5f, 20, 1f ) { res =>
         if( verbose ) println( name + ": search result : " + res )
         val jobs = res.map { tup =>
            val (idx, corr)   = tup
            val (start, stop) = isolateHit( idx )
            val rvsLen        = math.min( 44100L, stop - start )
            val inF           = AudioFile.openRead( p.inPath )
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

            Bleach(
               in       = outF.file.get.getAbsolutePath,
               out      = outPath.getAbsolutePath,
               spec     = OutputSpec.aiffFloat,
               gain     = Gain.immediate,
               length   = 256,
               feedback = "-50dB",
               clip     = "18dB"
            )
         }
         FScape.fsc.processChain( name, jobs ) {
            case true => (new Thread { override def run {
               jobs.foreach { bleach =>
                  ProcTxn.atomic { implicit tx => FScape.inject( new File( bleach.out ), "O-one" )}
                  Thread.sleep( (Util.exprand( 0.2, 1.5 ) * 1000).toLong )
               }
            }}).start

            case _ => println( name + ": FScape failed!" )
         }
      }
   }

   private def isolateHit( idx: Int ) : (Long, Long) = {
      (idx * anaWinStep, (idx + 86) * anaWinStep) // XXX TODO
   }
}
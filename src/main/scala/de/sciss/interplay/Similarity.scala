/*
 *  Similarity.scala
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
import SoundProcesses._
import de.sciss.math.SVD
import java.io.File
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

object Similarity {
   sealed trait Type

   // principal component analysis through singular value decomposition
   case object PCA extends Type

   // normalized cross correleation ( http://en.wikipedia.org/wiki/Cross-correlation )
   case object CC extends Type

   def test( off: Int, svnFrames: Int, normalize: Boolean = false, tpe: Type = CC ) {
      Actor.actor( act( off, svnFrames, normalize, tpe ))
   }

   private def act( off0: Int, svnFrames0: Int, normalize: Boolean, tpe: Type ) {
      println( "SVD STARTED" )

      val path       = File.createTempFile( "svd", ".aif", new File( System.getProperty( "user.home" ), "Desktop" ))
      val af         = AudioFile.openWrite( path, AudioFileSpec( numChannels = 1, sampleRate = 44100.0 / anaWinStep ))
      val afBuf      = af.frameBuffer( 1024 )
      val afChan     = afBuf( 0 )
      var bufPos     = 0

      val buf        = anaClientBuf
      val numFrames  = buf.framesWritten
      val srcFrame   = buf.emptyFrame
      val dstFrame   = buf.emptyFrame
      val m          = buf.numChannels

      // ---- normalize ----
      if( normalize ) {
         val dmin = new Array[Double]( m )
         val dmax = new Array[Double]( m )
         buf.getFrame( 0, srcFrame )
         var y = 0; while( y < m ) {
             dmin(y)=srcFrame(y); dmax(y)=srcFrame(y)
         y += 1 }
         var x = 1; while( x < numFrames ) {
             buf.getFrame( x, srcFrame )
             var y = 0; while( y < m ) {
                 dmin(y)=math.min(dmin(y),srcFrame(y)); dmax(y)=math.max(dmax(y),srcFrame(y))
             y += 1 }
         x += 1 }
         x = 0; while( x < numFrames ) {
             buf.getFrame( x, srcFrame )
             var y = 0; while( y < m ) {
                 srcFrame(y) = ((srcFrame(y) - dmin(y)) / (dmax(y) - dmin(y))).toFloat
             y += 1 }
             buf.setFrame( x, srcFrame )
         x += 1 }
      }

      val off        = math.max( 0, math.min( numFrames - svnFrames0, off0 ))
      val n          = math.min( svnFrames0, numFrames - off )

      val process: Int => Unit = tpe match {
         case PCA =>
            val mat        = Array.ofDim[ Float ]( m, n )
            val ns         = math.min( m + 1, n )
            val s          = new Array[ Float ]( ns )
            val u          = Array.ofDim[ Float ]( m, math.min( m, n ))

            def readAndDecompose( off: Int, frame: AnalysisBuffer.Frame ) {
               var idx = off; var x = 0; while( x < n ) {
                  buf.getFrame( idx, frame )
                  var y = 0; while( y < m ) {
                     mat( y )( x ) = frame( y )
                  y += 1 }
               idx += 1; x += 1 }

               // result vector is (0 until m).map( u => u( i )( 0 ) * s( 0 ))
               SVD.svd( mat, s, u, null )
               val gain = s( 0 )
               var y = 0; while( y < m ) {
                  frame( y ) = u( y )( 0 ) * gain
               y += 1 }
            }

            readAndDecompose( off, srcFrame )

            (off: Int) => {
               readAndDecompose( off, dstFrame )
               var y = 0; var difsum = 0.0; while( y < m ) {
                  val src = srcFrame( y )
                  val dst = dstFrame( y )
                  if( src != 0 ) {
                     val d = (dst - src) / src
                     difsum += d * d
                  }
               y += 1 }
               afChan( bufPos ) = (difsum / m).toFloat
            }

         case CC =>
            val srcMat  = Array.ofDim[ Float ]( n, m )
            val dstMat  = Array.ofDim[ Float ]( n, m )
            val matSize = n * m

            def readMat( off: Int, mat: Array[ Array[ Float ]]) {
               var idx = off; var x = 0; while( x < n ) {
                  buf.getFrame( idx, mat( x ))
               x += 1; idx += 1 }
            }

            def stat( mat: Array[ Array[ Float ]]) = {
               var sum = 0.0
               var x = 0; while( x < n ) {
                  val frame = mat( x )
                  var y = 0; while( y < m ) {
                     sum += frame( y )
                  y += 1 }
               x += 1 }
               val mean = sum / matSize
               sum = 0.0
               x = 0; while( x < n ) {
                  val frame = mat( x )
                  var y = 0; while( y < m ) {
                     val d = frame( y ) - mean
                     sum += d * d
                  y += 1 }
               x += 1 }
               val stddev = math.sqrt( sum / matSize )
               (mean, stddev)
            }

            def norm( mat: Array[ Array[ Float ]], mean: Double, stddev: Double ) {
               val add = -mean
               val mul = 1.0 / stddev
               var x = 0; while( x < n ) {
                  val frame = mat( x )
                  var y = 0; while( y < m ) {
                     frame( y ) = ((frame( y ) + add) * mul).toFloat
                  y += 1 }
               x += 1 }
            }

            def prepare( off: Int, mat: Array[ Array[ Float ]]) {
               readMat( off, mat )
               val (mean, stddev) = stat( mat )
               norm( mat, mean, stddev )
            }

            prepare( off, srcMat )

            (off: Int) => {
               prepare( off, dstMat )
               var sum = 0.0
               var x = 0; while( x < n ) {
                  val srcFrame = srcMat( x )
                  val dstFrame = dstMat( x )
                  var y = 0; while( y < m ) {
                     sum += srcFrame( y ) * dstFrame( y )
                  y += 1 }
               x += 1 }
               afChan( bufPos ) = (sum / (matSize - 1)).toFloat
            }
      }

      var x = 0; while( x < numFrames - n ) {
         process( x )

//         var y = 0; var sqrsum = 0.0; while( y < m ) {
//            val src = srcFrame( y )
//            val dst = dstFrame( y )
//            if( src != 0 ) {
//               val d = (dst - src) / src
//               sqrsum += d * d
//            }
//         y += 1 }

//         var y = 0; var sqrsum = 0.0; var srcsum = 0.0; while( y < m ) {
//            val src = srcFrame( y )
//            val dst = dstFrame( y )
//            if( src != 0 ) {
//               val d = (dst - src) / src
//               sqrsum += d * d
//               srcsum += src * src
//            }
//         y += 1 }
//         val rms  = sqrsum / (srcsum * m)
//         afChan( bufPos ) = if( srcsum > 0 ) (-math.log( rms ) / 1024).toFloat else {
//            println( "woop, zero" )
//            144f
//         }
         bufPos += 1
         if( bufPos == 1024 ) {
            af.writeFrames( afBuf )
            println( "---- " + af.numFrames )
            bufPos = 0
         }
      x += 1 }

      af.writeFrames( afBuf, 0, bufPos )
      println( "---- " + af.numFrames + " : DONE!" )
      af.close
   }
}
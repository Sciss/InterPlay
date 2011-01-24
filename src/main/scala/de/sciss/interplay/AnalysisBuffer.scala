/*
 *  AnalysisBuffer.scala
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

import java.nio.{ByteBuffer, FloatBuffer}
import de.sciss.synth.Model

object AnalysisBuffer {
   case class FrameUpdated( idx: Int, lastFrame: Boolean )
   type Frame = Array[ Float ]
}

class AnalysisBuffer( val numFrames: Int, val numChannels: Int ) extends Model {
   import AnalysisBuffer._

   private val buf = ByteBuffer.allocateDirect( numFrames * numChannels * 4 ).asFloatBuffer
   private var framesWrittenVar = 0

   def emptyFrame : Frame = new Array[ Float ]( numChannels )
//   def view : FloatBuffer = buf.duplicate
   def setFrame( idx: Int, content: Frame ) {
      val lastFrame = buf.synchronized {
         buf.position( idx * numChannels )
         buf.put( content )
         val res = idx == framesWrittenVar
         if( res ) {
            framesWrittenVar = idx + 1
         }
         res
      }
      dispatch( FrameUpdated( idx, lastFrame ))
   }

   def framesWritten : Int = buf.synchronized( framesWrittenVar )

   def getFrame( idx: Int, holder: Frame = emptyFrame ) : Frame = {
      buf.synchronized {
         buf.position( idx * numChannels )
         buf.get( holder )
      }
      holder
   }
}
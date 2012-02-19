/*
 *  AnalysisView.scala
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

import java.nio.FloatBuffer
import javax.swing.{JFrame, JComponent}
import java.awt.image.BufferedImage
import de.sciss.sonogram.IntensityColorScheme
import java.awt.event.{MouseEvent, MouseAdapter}
import de.sciss.synth.Model
import de.sciss.gui.{Collapse, DynamicAncestorAdapter}
import java.awt.{BorderLayout, Graphics, Color, Insets, Dimension}

class AnalysisView( buf: AnalysisBuffer, marks: AnalysisMarkers, width: Int, height: Int )
extends JComponent {
   view =>

   private val numFrames   = buf.numFrames
   private val downSmp     = math.max( 1, numFrames / width )
   private val imgWidth    = (numFrames + downSmp - 1) / downSmp
   private val mrkIdx      = buf.numChannels
   private var img: BufferedImage = null // = new BufferedImage( nWidth, buf.numChannels, BufferedImage.TYPE_INT_RGB )
   private val ins         = new Insets( 0, 0, 0, 0 )
   private val sum         = buf.emptyFrame
   private val tmp         = buf.emptyFrame

//   println( "AnaView : numFrames = " + numFrames + "; downSmp = " + downSmp + "; img = " + img.getWidth + ", " + img.getHeight )

//   rebuildImage

   setPreferredSize( new Dimension( width, height ))
//   addMouseListener( new MouseAdapter {
//      override def mousePressed( e: MouseEvent ) {
//         if( e.getClickCount == 2 ) rebuildImage
//      }
//   })

   private val clpseBuf = Collapse[ (Int, Int) ]( 1.0 ) { (a, b) => (math.min( a._1, b._1 ), math.max( a._2, b._2 ))} { res =>
//      println( "UPDATE " + res )
      rebuildImage( res._1, res._2 )
   }

   private val clpseMrk = Collapse[ List[ Int ]]( 0.1 )( _ ++ _ )( addMarkers( _ ))

   private val bufListener: Model.Listener = {
      case AnalysisBuffer.FrameUpdated( idx, isLast ) => clpseBuf( (idx, idx + 1) )
   }

   private val marksListener: Model.Listener = {
      case AnalysisMarkers.Added( idx ) => clpseMrk( idx :: Nil )
   }

   DynamicAncestorAdapter( view ) {
      img = new BufferedImage( imgWidth, buf.numChannels + 1, BufferedImage.TYPE_INT_RGB )
      rebuildImage( 0, buf.framesWritten )
      buf.addListener( bufListener )
      marks.addListener( marksListener )
   } {
      clpseBuf.cancel()
      clpseMrk.cancel()
      buf.removeListener( bufListener )
      marks.removeListener( marksListener )
      img.flush
      img = null
   }

   private def addMarkers( marks: List[ Int ]) {
      getInsets( ins )
      val w    = getWidth  - (ins.left + ins.right)
      val h    = getHeight - (ins.top  + ins.bottom)

      var x0 = w
      var x1 = 0
      marks.foreach { m =>
         val x = m / downSmp
         img.setRGB( x, mrkIdx, 0xFFFFFF )
         if( x >= x1 ) x1 = x + 1
         if( x < x0 )  x0 = x
      }
      if( x0 < x1 ) {
         val xi0 = x0 * w / imgWidth
         val xi1 = (x1 * w + imgWidth - 1) / imgWidth
         repaint( 0L, xi0 + ins.left, ins.top, xi1 - xi0, h )
      }
   }

   private def rebuildImage( startFrame: Int, stopFrame: Int ) {
      if( img == null ) return

      getInsets( ins )
      val w    = getWidth  - (ins.left + ins.right)
      val h    = getHeight - (ins.top  + ins.bottom)

      val x0 = startFrame / downSmp
      val x1 = (stopFrame + downSmp - 1) / downSmp
      val imgH = buf.numChannels
      var x = x0; var frame = x0 * downSmp; while( frame < stopFrame ) {
         buf.getFrame( frame, sum )
         frame += 1
         var i = 1; while( i < downSmp && frame < stopFrame ) {
            buf.getFrame( frame, tmp )
            var y = 0; while( y < imgH ) {
               sum( y ) += tmp( y )
            y += 1 }
         i += 1; frame += 1 }
         var y = 0; while( y < buf.numChannels ) {
//            img.setRGB( x, y, IntensityColorScheme( sum( y ) / downSmp ))
            img.setRGB( x, imgH - 1 - y, IntensityColorScheme( sum( y ) / downSmp ))
         y += 1 }
      x += 1 }

      val xi0 = x0 * w / imgWidth
      val xi1 = (x1 * w + imgWidth - 1) / imgWidth

      repaint( 0L, xi0 + ins.left, ins.top, xi1 - xi0, h )
   }

   override def paintComponent( g: Graphics ) {
      super.paintComponent( g )
      if( img == null ) return

      getInsets( ins )
      val w = getWidth - (ins.left + ins.right)
      val h = getHeight - (ins.top + ins.bottom)
//      println( "w = " + w + "; h = " + h )
      g.drawImage( img, ins.left, ins.top, w, h, view )
   }

   def makeWindow : JFrame = {
      val f    = new JFrame( "Analysis" )
      val cp   = f.getContentPane
      cp.add( this, BorderLayout.CENTER )
      f.pack()
      f.setLocationRelativeTo( null )
      f.setVisible( true )
      f
   }
}
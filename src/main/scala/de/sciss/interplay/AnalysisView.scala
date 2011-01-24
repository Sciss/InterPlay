package de.sciss.interplay

import java.nio.FloatBuffer
import javax.swing.{JFrame, JComponent}
import java.awt.image.BufferedImage
import de.sciss.sonogram.IntensityColorScheme
import java.awt.{BorderLayout, Insets, Dimension, Graphics}
import java.awt.event.{MouseEvent, MouseAdapter}

class AnalysisView( buf: FloatBuffer, numChannels: Int, width: Int, height: Int ) extends JComponent {
   private val numFrames   = buf.capacity / numChannels
   private val downSmp     = math.max( 1, numFrames / width )
   private val nWidth      = numFrames / downSmp // (numFrames + downSmp - 1) / downSmp
   private val img         = new BufferedImage( nWidth, numChannels, BufferedImage.TYPE_INT_RGB )
   private val ins         = new Insets( 0, 0, 0, 0 )

//   println( "AnaView : numFrames = " + numFrames + "; downSmp = " + downSmp + "; img = " + img.getWidth + ", " + img.getHeight )

   rebuildImage

   setPreferredSize( new Dimension( width, height ))
   addMouseListener( new MouseAdapter {
      override def mousePressed( e: MouseEvent ) {
         if( e.getClickCount == 2 ) rebuildImage
      }
   })

   private def rebuildImage {
      buf.clear()
      val sum = new Array[ Float ]( numChannels )
      val tmp = new Array[ Float ]( numChannels )
      var x = 0; while( x < nWidth ) {
         buf.get( sum )
         var i = 1; while( i < downSmp ) {
            buf.get( tmp )
            var y = 0; while( y < numChannels ) {
               sum( y ) += tmp( y )
            y += 1 }
         i += 1 }
         var y = 0; while( y < numChannels ) {
            img.setRGB( x, y, IntensityColorScheme( sum( y ) / downSmp ))
         y += 1 }
      x += 1 }
      repaint()
   }

   override def paintComponent( g: Graphics ) {
      super.paintComponent( g )
      getInsets( ins )
      val w = getWidth - (ins.left + ins.right)
      val h = getHeight - (ins.top + ins.bottom)
//      println( "w = " + w + "; h = " + h )
      g.drawImage( img, ins.left, ins.top, w, h, this )
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
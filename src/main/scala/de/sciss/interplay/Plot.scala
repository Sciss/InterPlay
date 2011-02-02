package de.sciss.interplay

import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import javax.swing.{JComponent, WindowConstants, JFrame}
import java.awt.geom.GeneralPath
import java.awt.{RenderingHints, Graphics, Graphics2D, BorderLayout}

object Plot {
   def doubles( pts: (Double, Double)* )  = new Plot( pts.map( tup => tup._1.toFloat -> tup._2.toFloat )( breakOut ))
   def apply( pts: (Float, Float)* )      = new Plot( pts.toIndexedSeq )
}
class Plot( val pts: IIdxSeq[ (Float, Float )]) {
   private lazy val (minX, minY, maxX, maxY) = pts.tail.foldLeft( (pts.head._1, pts.head._2, pts.head._1, pts.head._2) )( (m, pt) => {
      val (minX, minY, maxX, maxY) = m
      val (x, y) = pt
      (math.min( minX, x ), math.min( minY, y ), math.max( maxX, x ), math.max( maxY, y ))
   })
   private lazy val scaleX = { val d = maxX - minX; 1f / (if( d > 0 ) d else 1f) }
   private lazy val scaleY = { val d = maxY - minY; 1f / (if( d > 0 ) d else 1f) }

   val frame = {
      val f = new JFrame( "Plot" )
      f.setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE )
      f.setSize( 400, 400 )
      f.setLocationRelativeTo( null )
      val cp = f.getContentPane()
      cp.add( Panel, BorderLayout.CENTER )
      f.setVisible( true )
      f
   }

   private object Panel extends JComponent {
      val shp = new GeneralPath()
      var lastW = -1
      var lastH = -1
      override def paintComponent( g: Graphics ) {
         super.paintComponent( g )
         val g2 = g.asInstanceOf[ Graphics2D ]
         val w = getWidth()
         val h = getHeight()
         g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON )
         if( w != lastW || h != lastH ) {
            shp.reset()
            val wm = w - 1
            val hm = h - 1
            pts.foreach { pt =>
               val (x, y) = pt
               val xn = (x - minX) * scaleX * wm
               val yn = (1f - ((y - minY) * scaleY)) * hm
               shp.moveTo( xn - 4, yn )
               shp.lineTo( xn + 4, yn )
               shp.moveTo( xn, yn - 4 )
               shp.lineTo( xn, yn + 4 )
            }
            lastW = w
            lastH = h
         }
         g2.draw( shp )
      }
   }
}
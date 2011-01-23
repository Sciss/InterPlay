package de.sciss.interplay

import de.sciss.gui.RecessedBorder
import javax.swing.border.CompoundBorder
import javax.swing.{BorderFactory, JLabel, JPanel}
import java.util.{TimerTask, Timer}
import java.awt.{EventQueue, Graphics, Color, Font}

class Wallclock extends JLabel {
   private var secs = 0
   private val timer = new Timer( true )
   private var running = false
   private val tt = new TimerTask {
      def run = EventQueue.invokeLater( new Runnable { def run = tick })
   }
   private val sb = new StringBuilder( 6 )

   setBorder( BorderFactory.createCompoundBorder( new RecessedBorder, BorderFactory.createMatteBorder( 0, 4, 0, 4, Color.black )))
   setFont( new Font( "Menlo", Font.PLAIN, 16 ))
   setBackground( Color.black )
   setForeground( Color.white )

   updateLabel
   setMinimumSize( getPreferredSize() )
   setMaximumSize( getPreferredSize() )

   private def updateLabel {
      val secs0   = math.min( 5999, math.max( 0, secs ))
      val mins    = secs0 / 60
      val secs1   = secs0 % 60
      sb.delete( 0, 6 )
      sb.append( ((mins / 10) + '0').toChar )
      sb.append( ((mins % 10) + '0').toChar )
      sb.append( if( secs1 % 2 == 0 ) ' ' else ':' )
      sb.append( ((secs1 / 10) + '0').toChar )
      sb.append( ((secs1 % 10) + '0').toChar )
      setText( sb.toString )
   }

   def start {
      if( !running ) {
         running = true
         timer.scheduleAtFixedRate( tt, 1000L, 1000L )
      }
   }

   def stop {
      running = false
      timer.cancel()
   }

   def reset {
      stop
      secs = 0
      updateLabel
   }

   private def tick {
      if( running ) {
         secs += 1
         updateLabel
      }
   }

   override def paintComponent( g: Graphics ) {
      val in = getInsets()
      g.setColor( getBackground() )
      g.fillRect( in.left, in.top, getWidth() - (in.left + in.right), getHeight() - (in.top + in.bottom ))
      super.paintComponent( g )
   }
}
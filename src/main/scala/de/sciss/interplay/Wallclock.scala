/*
 *  Wallclock.scala
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

import de.sciss.gui.RecessedBorder
import javax.swing.border.CompoundBorder
import javax.swing.{BorderFactory, JLabel, JPanel}
import java.util.{TimerTask, Timer}
import java.awt.{EventQueue, Graphics, Color, Font}

class Wallclock extends JLabel {
   private var secs = 0
   private var timer: Option[ Timer ] = None // = new Timer( true )
//   private var running = false
//   private val tt = new TimerTask {
//      def run = EventQueue.invokeLater( new Runnable { def run = tick })
//   }
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
      require( EventQueue.isDispatchThread )
      if( timer.isEmpty ) {
         val t = new Timer( true )
         t.scheduleAtFixedRate( new TimerTask {
            def run = EventQueue.invokeLater( new Runnable { def run {
               if( timer == Some( t )) {
                  secs += 1
                  updateLabel
               }
            }})
         }, 1000L, 1000L )
         timer = Some( t )
      }
   }

   def stop {
      require( EventQueue.isDispatchThread )
      timer.foreach { t =>
         t.cancel()
         timer = None
      }
   }

   def reset {
      stop
      secs = 0
      updateLabel
   }

//   def isRunning = timer.isDefined

//   private def tick {
//      if( running ) {
//         secs += 1
//         updateLabel
//      }
//   }

   override def paintComponent( g: Graphics ) {
      val in = getInsets()
      g.setColor( getBackground() )
      g.fillRect( in.left, in.top, getWidth() - (in.left + in.right), getHeight() - (in.top + in.bottom ))
      super.paintComponent( g )
   }
}
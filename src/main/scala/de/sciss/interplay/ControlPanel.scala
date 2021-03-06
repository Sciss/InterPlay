/*
 *  ControlPanel.scala
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

import java.awt.event.{ComponentEvent, ComponentAdapter, WindowAdapter, ActionListener, ActionEvent}
import de.sciss.gui.{PeakMeterPanel, PeakMeter, PeakMeterGroup}
import InterPlay._
import SoundProcesses._
import de.sciss.scalainterpreter.LogPane
import java.awt.{Font, Color, BorderLayout}
import java.io.PrintStream
import javax.swing._
import de.sciss.synth.proc.{Proc, ProcTxn}

class ControlPanel() extends JPanel {
   panel =>

   private val masterMeterPanel  = new PeakMeterPanel()
   private val peopleOffset      = masterBus.numChannels << 1
   private val peopleMeterPanel: Option[ PeakMeterPanel ] =
      if( MIC_AND_PEOPLE.nonEmpty ) Some( new PeakMeterPanel() ) else None

   private var interpreter : Option[ ScalaInterpreterFrame ] = None
   private val ggClock = new Wallclock

   {
      panel.setLayout( new BoxLayout( panel, BoxLayout.X_AXIS ))

//      val ggTapes = new JToggleButton( "Tapes" )
//      ggTapes.putClientProperty( "JButton.buttonType", "bevel" )
//      ggTapes.putClientProperty( "JComponent.sizeVariant", "small" )
//      ggTapes.setFocusable( false )
//      ggTapes.addActionListener( new ActionListener {
//         def actionPerformed( e: ActionEvent ) {
//            val sel = ggTapes.isSelected()
//            tapesFrame.setVisible( sel )
//            if( sel ) tapesFrame.toFront()
//         }
//      })
//      tapesFrame.addComponentListener( new ComponentAdapter {
//         override def componentHidden( e: ComponentEvent ) {
//            ggTapes.setSelected( false )
//         }
//      })

//      panel.add( Box.createHorizontalStrut( 4 ))
//      panel.add( ggTapes )
//      panel.add( Box.createHorizontalStrut( 4 ))

//      val m1 = new PeakMeter( SwingConstants.HORIZONTAL )
//      val m2 = new PeakMeter( SwingConstants.HORIZONTAL )
//      val mg = new PeakMeterGroup( Array( m1, m2 ))
//      panel.add( m1 )
//      panel.add( m2 )

      def dressTiny( b: AbstractButton, label: String )( action: => Unit ) {
//         b.putClientProperty( "JButton.buttonType", "bevel" )
         b.putClientProperty( "JComponent.sizeVariant", "small" )
         b.setFocusable( false )
         b.setAction( new AbstractAction( label ) {
            def actionPerformed( e: ActionEvent ) { action }
         })
      }
      def tinyButton( label: String )( action: => Unit ) : JButton = {
         val b = new JButton()
         dressTiny( b, label )( action )
         b.putClientProperty( "JButton.buttonType", "bevel" )
         b
      }
      def tinyToggle( label: String )( action: Boolean => Unit ) : JToggleButton = {
         val b = new JToggleButton()
         dressTiny( b, label )( action( b.isSelected ))
         b.putClientProperty( "JButton.buttonType", "square" )
         b
      }
      def space( px: Int = 4 ) { panel.add( Box.createHorizontalStrut( px ))}

      panel.add( tinyButton( "\u25B6" ) {
         Process.spawnAtomic( "startLive button" ) { implicit tx => SoundProcesses.startLive }
      })
      panel.add( tinyButton( "\u25FC" ) {
         Process.spawnAtomic( "stopProcesses button" ) { implicit tx => SoundProcesses.stopProcesses }
      })
      panel.add( ggClock )
      space()
      panel.add( tinyToggle( "Rec" )( b => Process.spawnAtomic( "Rec button" ) { implicit tx => SoundProcesses.mitschnitt( b )}))
      space( 16 )
      panel.add( tinyToggle( "HP" )( SoundProcesses.headphoneMix( _ )))
//      List( ("StringBleach", "string"), ("GlissBleach", "gliss") ) foreach { tup =>
//         val (name, tempName) = tup
//         space()
//         panel.add( tinyButton( tempName.capitalize ) {
//            (Similarity.templates.get( tempName ), playPath) match {
//               case (Some( temp ), Some( inPath )) => spawnAtomic( ProcTasten.perform( temp, inPath )( _ ))
//               case _ =>
//            }
//         })
//      }

      val numCh = masterBus.numChannels
      masterMeterPanel.setOrientation( SwingConstants.HORIZONTAL )
      masterMeterPanel.setNumChannels( numCh )
      masterMeterPanel.setBorder( true )
      val d = masterMeterPanel.getPreferredSize()
      val dn = 30 / numCh
      d.height = numCh * dn + 7
      masterMeterPanel.setPreferredSize( d )
      masterMeterPanel.setMaximumSize( d )
      panel.add( masterMeterPanel )
      peopleMeterPanel.foreach { p =>
         p.setOrientation( SwingConstants.HORIZONTAL )
         p.setNumChannels( MIC_AND_PEOPLE.size )
         p.setBorder( true )
         val d = p.getPreferredSize()
         val dn = 30 / numCh
         d.height = numCh * dn + 7
         p.setPreferredSize( d )
         p.setMaximumSize( d )
         panel.add( p )
      }

      val d1 = logPane.component.peer.getPreferredSize()
      d1.height = d.height
      logPane.component.peer.setPreferredSize( d1 )
      space( 8 )
      panel.add( logPane.component.peer)
      space( 16 )

      val glue = Box.createHorizontalGlue()
//      glue.setBackground( Color.darkGray )
      panel.add( glue )

      val ggInterp = new JToggleButton( "REPL" )
      ggInterp.putClientProperty( "JButton.buttonType", "bevel" )
      ggInterp.putClientProperty( "JComponent.sizeVariant", "small" )
      ggInterp.setFocusable( false )
      ggInterp.addActionListener( new ActionListener {
         def actionPerformed( e: ActionEvent ) {
            val sel = ggInterp.isSelected()
            if( sel ) {
               val f = interpreter.getOrElse {
                  val res = new ScalaInterpreterFrame( support /* ntp */ )
                  interpreter = Some( res )
                  res.setAlwaysOnTop( true )
                  res.setDefaultCloseOperation( WindowConstants.HIDE_ON_CLOSE )
                  res.addComponentListener( new ComponentAdapter {
                     override def componentHidden( e: ComponentEvent ) {
                        ggInterp.setSelected( false )
                     }
                  })
                  // need to restore console for some reason
                  Console.setErr( System.err )
                  Console.setOut( System.out )
                  res
               }
               f.setVisible( true )
            } else interpreter.foreach( _.setVisible( false ))
         }
      })
      panel.add( ggInterp )
   }

   def makeWindow : JFrame = makeWindow()
   def makeWindow( undecorated: Boolean = true ) : JFrame = {
      val f = new JFrame( "Nuages Controls" )
      if( undecorated ) f.setUndecorated( true )
//      val cp = f.getContentPane()
//      cp.add( panel, BorderLayout.CENTER )
      f.setContentPane( panel )
      f.pack()
      f
   }

   def init( implicit tx: ProcTxn ) {
      pLive.addListener( new Proc.Listener {
         var sawRunning = false
         def updated( u: Proc.Update ) {
            if( u.state.playing && !sawRunning ) {
               sawRunning = true
               guiRun( startClock )
            }
         }
      })
   }

   private def stopClock {
      ggClock.stop
   }

   private def startClock {
      ggClock.reset
      ggClock.start
   }

   def meterUpdate( peakRMSPairs: Array[ Float ]) {
      val tim = System.currentTimeMillis
      masterMeterPanel.meterUpdate( peakRMSPairs, 0, tim )
      peopleMeterPanel.foreach( _.meterUpdate( peakRMSPairs, peopleOffset, tim ))
   }
}
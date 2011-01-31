/*
 *  InterPlay.scala
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

import de.sciss.synth.swing.{NodeTreePanel, ServerStatusPanel}
import actors.Actor
import de.sciss.synth.proc.{ProcDemiurg, ProcTxn}
import de.sciss.synth.{ServerOptionsBuilder, ServerConnection, AudioBus, Server}
import collection.immutable.{IndexedSeq => IIdxSeq}
import java.awt.{BorderLayout, Font, GraphicsEnvironment, EventQueue}
import de.sciss.scalainterpreter.LogPane
import javax.swing.{Box, JScrollPane, WindowConstants, JFrame}
import de.sciss.synth.osc.OSCResponder
import de.sciss.osc.OSCMessage
import java.io.{File, PrintStream}
import SoundProcesses._
import de.sciss.nuages.{NuagesPanel, NuagesConfig, NuagesFrame}

object InterPlay {
//   NuagesPanel.verbose = true

   var s: Server  = null
//   var gui: GUI   = null
   var booting: ServerConnection = null
//   val options = new ServerOptionsBuilder()
   var ntp: Option[ NodeTreePanel ] = None
   var masterBus: AudioBus = null
//   var headphonesBus: AudioBus = null
   val INTERNAL_AUDIO      = true
   val NUAGES_ANTIALIAS    = false
   val MASTER_OFFSET       = 2
   val MASTER_NUMCHANNELS  = 5
   val SOLO_OFFSET         = 0
   val SOLO_NUMCHANNELS    = 2
   val SAMPLE_RATE         = 44100.0

   val support             = new REPLSupport
   val PEOPLE_CHANGROUPS   = List.empty[ (String, Int, Int) ]
   val REC_CHANGROUPS      = List.empty[ (String, Int, Int) ] // List( "hp-mix", 10, 2 )
   val MIC_OFFSET          = 0
   val MIC_NUMCHANNELS     = 1
   lazy val MIC_AND_PEOPLE = ("Mic", MIC_OFFSET, MIC_NUMCHANNELS) :: PEOPLE_CHANGROUPS

   val BASE_PATH           = new File( new File( System.getProperty( "user.home" ), "Desktop" ), "InterPlay" )
   lazy val REC_PATH       = new File( BASE_PATH, "rec" )
   lazy val TEMPLATE_PATH  = new File( BASE_PATH, "templates" )
   lazy val FSC_PATH       = new File( REC_PATH, "fsc" )

   val USE_MIDI            = true

   var LIVE_FILE           = None // Some( "live110125_143645.irc" ) // Some( "live110128_113557.irc" ) // Some( "live110128_121639.irc" )
   val LIVE_MODE           = if( LIVE_FILE.isDefined ) 1 else 0

//   println( "MIC_AND_PEOPLE = " + MIC_AND_PEOPLE )

//   val MASTER_CHANNELS = IIdxSeq( 0, 1 )
//   val HEADPHONES_INDEX = 0
   var config: NuagesConfig = null

   val inDevice         = "MOTU 828mk2"
   val outDevice        = "MOTU 828mk2"

   lazy val options     = {
      val o          = new ServerOptionsBuilder()
      if( inDevice == outDevice ) {
         if( inDevice != "" ) o.deviceName = Some( inDevice )
      } else {
         o.deviceNames = Some( inDevice -> outDevice )
      }

      val maxInIdx = MIC_AND_PEOPLE.map( g => g._2 + g._3 ).max

      val maxOutIdx = ((MASTER_OFFSET + MASTER_NUMCHANNELS) :: (if( SOLO_OFFSET >= 0 ) SOLO_OFFSET + SOLO_NUMCHANNELS else 0) ::
         REC_CHANGROUPS.map( g => g._2 + g._3 )).max

//      println( "MAX IN " + maxInIdx + " ; MAX OUT " + maxOutIdx )

      o.inputBusChannels   = maxInIdx // 10
      o.outputBusChannels  = maxOutIdx // 10
      o.audioBusChannels   = 512
      o.loadSynthDefs      = false
      o.memorySize         = 65536
      o.zeroConf           = false
      o.build
   }

   lazy val SCREEN_BOUNDS =
         GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration.getBounds

   lazy val logPane = {
      val res = new LogPane( 2, 30 )
      res.init
      res.getComponent( 0 ) match {
         case scroll: JScrollPane =>
            scroll.setBorder( null )
            scroll.getViewport().getView().setFont( new Font( "Menlo", Font.PLAIN, 8 ))
         case _ => println( "Ooops, no scrollpane" )
      }
      val printStream = new PrintStream( res.outputStream )
      System.setErr( printStream )
      System.setOut( printStream )
//      ggLog.writer.write( "Make noise.\n" )
      Console.setErr( res.outputStream )
      Console.setOut( res.outputStream )
      res
   }

   def main( args: Array[ String ]) {
      guiRun { init }
   }

   def guiRun( code: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run = code })
   }

//   def init {
//      val f = new JFrame( "InterPlay" )
//      f.setResizable( false )
//      f.setSize( 200, 200 )
//      f.setLocationRelativeTo( null )
//      f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
//      f.setVisible( true )
//   }

   def init {
      // prevent actor starvation!!!
      // --> http://scala-programming-language.1934581.n4.nabble.com/Scala-Actors-Starvation-td2281657.html
      System.setProperty( "actors.enableForkJoin", "false" )

//      val sif  = new ScalaInterpreterFrame( support /* ntp */ )
      val ssp  = new ServerStatusPanel( ServerStatusPanel.COUNTS )
//      val sspw = ssp.makeWindow( undecorated = true )
//      sspw.pack()

      val maxX = SCREEN_BOUNDS.x + SCREEN_BOUNDS.width - 48
      val maxY = SCREEN_BOUNDS.y + SCREEN_BOUNDS.height - 35 /* sspw.getHeight() + 3 */
//      sspw.setLocation( SCREEN_BOUNDS.x - 3, maxY - 1 )

//      val ntp  = new NodeTreePanel()
//      val ntpw = ntp.makeWindow
//      ntpw.setLocation( sspw.getX, sspw.getY + sspw.getHeight + 32 )
//      sspw.setVisible( true )
//      ntpw.setVisible( true )

      Similarity.templates // initializes them

//      sif.setLocation( sspw.getX + sspw.getWidth + 32, sif.getY )
//      sif.setVisible( true )
      booting = Server.boot( options = options ) {
         case ServerConnection.Preparing( srv ) => {
            ssp.server = Some( srv )
//            ntp.server = Some( srv )
         }
         case ServerConnection.Running( srv ) => guiRun {
            ProcDemiurg.addServer( srv )
            s = srv
            support.s = srv

            // nuages
            initNuages( maxX, maxY )

            // freesound
            val ctrlP = new ControlPanel()
//      val cf = ctrlP.makeWindow
            val ctrlF = new JFrame()
            ctrlF.setUndecorated( true )
            val ctrlB = Box.createHorizontalBox()
            ctrlB.add( ssp )
            ctrlB.add( Box.createHorizontalStrut( 8 ))
            ctrlB.add( ctrlP )
            ctrlB.add( Box.createHorizontalStrut( 4 ))
            ctrlF.setContentPane( ctrlB )
            ctrlF.pack()
            ctrlF.setBounds( SCREEN_BOUNDS.x - 1, SCREEN_BOUNDS.y + SCREEN_BOUNDS.height - ctrlF.getHeight() + 2,
               maxX - SCREEN_BOUNDS.x + 1, ctrlF.getHeight() )
            ctrlF.setVisible( true )

            val synPostMID = synPostM.id
            OSCResponder.add({
               case OSCMessage( "/meters", `synPostMID`, 0, values @ _* ) =>
                  EventQueue.invokeLater( new Runnable { def run = ctrlP.meterUpdate( values.map( _.asInstanceOf[ Float ]).toArray )})
            }, s )
         }
      }
      Runtime.getRuntime().addShutdownHook( new Thread { override def run = shutDown })
//      booting.start
   }

   private def initNuages( maxX: Int, maxY: Int ) {
//      masterBus  = if( INTERNAL_AUDIO ) {
//         new AudioBus( s, 0, 2 )
//      } else {
//         new AudioBus( s, MASTER_INDEX, MASTER_NUMCHANNELS )
//      }
//      val soloBus    = Bus.audio( s, 2 )
      val masterChans  = (MASTER_OFFSET until (MASTER_OFFSET + MASTER_NUMCHANNELS ))
//      headphonesBus  = new AudioBus( s, HEADPHONES_INDEX, 2 )
      val soloBus    = if( (SOLO_OFFSET >= 0) ) Some( (SOLO_OFFSET until (SOLO_OFFSET + SOLO_NUMCHANNELS)) ) else None
      config         = NuagesConfig( s, Some( masterChans ), soloBus, None, true )

      val f          = new NuagesFrame( config )
      masterBus      = f.panel.masterBus.get // XXX not so elegant
      f.panel.display.setHighQuality( NUAGES_ANTIALIAS )
      val y0 = SCREEN_BOUNDS.y + 22
      val y1 = y0 + 96
      f.setBounds( SCREEN_BOUNDS.x, y1, maxX - SCREEN_BOUNDS.x, maxY - y1 )
      f.setUndecorated( true )
      f.setVisible( true )
      support.nuages = f

      val anaView = new AnalysisView( anaClientBuf, anaMarkers, math.min( anaClientBuf.numFrames, maxX - SCREEN_BOUNDS.x ), 96 )
      val anaWin  = new JFrame()
      anaWin.setUndecorated( true )
      anaWin.setResizable( false )
      anaWin.getContentPane().add( anaView, BorderLayout.CENTER )
      anaWin.setBounds( SCREEN_BOUNDS.x, y0, maxX - SCREEN_BOUNDS.x, y1 - y0 )
      anaWin.setVisible( true )
//anaView.makeWindow

//      Actor.actor {
         ProcTxn.atomic { implicit tx =>
            SoundProcesses.init( s )
//         }
//         if( START_META ) ProcTxn.atomic { implicit tx =>
//            SemiNuages.meta.init
//         }
      }

      if( USE_MIDI ) Midi.init( f.panel )
   }

   def quit { System.exit( 0 )}

   private def shutDown { // sync.synchronized { }
      if( (s != null) && (s.condition != Server.Offline) ) {
         s.quit
         s = null
      }
      if( booting != null ) {
         booting.abort
         booting = null
      }
    }
}
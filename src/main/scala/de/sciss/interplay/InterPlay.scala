package de.sciss.interplay

import de.sciss.synth.swing.{NodeTreePanel, ServerStatusPanel}
import actors.Actor
import de.sciss.synth.proc.{ProcDemiurg, ProcTxn}
import de.sciss.synth.{ServerOptionsBuilder, ServerConnection, AudioBus, Server}
import de.sciss.nuages.{NuagesConfig, NuagesFrame}
import collection.immutable.{IndexedSeq => IIdxSeq}
import java.io.PrintStream
import java.awt.{Font, GraphicsEnvironment, EventQueue}
import de.sciss.scalainterpreter.LogPane
import javax.swing.{Box, JScrollPane, WindowConstants, JFrame}
import de.sciss.synth.osc.OSCResponder
import de.sciss.osc.OSCMessage

object InterPlay {
   var s: Server  = null
//   var gui: GUI   = null
   var booting: ServerConnection = null
   val options = new ServerOptionsBuilder()
   var ntp: Option[ NodeTreePanel ] = None
   var masterBus: AudioBus = null
   var headphonesBus: AudioBus = null
   val INTERNAL_AUDIO = true
   val NUAGES_ANTIALIAS = false
   val support = new REPLSupport
//   val MASTER_INDEX = 0
//   val MASTER_NUMCHANNELS = 2
   val MASTER_CHANNELS = IIdxSeq( 0, 1 )
   val HEADPHONES_INDEX = 0
   var config: NuagesConfig = null

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

//      sif.setLocation( sspw.getX + sspw.getWidth + 32, sif.getY )
//      sif.setVisible( true )
      booting = Server.boot( options = options.build ) {
         case ServerConnection.Preparing( srv ) => {
            ssp.server = Some( srv )
//            ntp.server = Some( srv )
         }
         case ServerConnection.Running( srv ) => {
            ProcDemiurg.addServer( srv )
            s = srv
            support.s = srv

            // nuages
            initNuages( maxX, maxY )

            // freesound
            val ctrlP = new ControlPanel( 0 )
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

//            val synPostMID = SMCNuages.synPostM.id
//            OSCResponder.add({
//               case OSCMessage( "/meters", `synPostMID`, 0, values @ _* ) =>
//                  EventQueue.invokeLater( new Runnable { def run = ctrlP.meterUpdate( values.map( _.asInstanceOf[ Float ]).toArray )})
//            }, s )

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
      headphonesBus  = new AudioBus( s, HEADPHONES_INDEX, 2 )
      config         = NuagesConfig( s, Some( MASTER_CHANNELS ), None, None ) // Some( RECORD_PATH )

      val f          = new NuagesFrame( config )
      masterBus      = f.panel.masterBus.get // XXX not so elegant
      f.panel.display.setHighQuality( NUAGES_ANTIALIAS )
      val y0 = SCREEN_BOUNDS.y + 22
      f.setBounds( SCREEN_BOUNDS.x, y0, maxX - SCREEN_BOUNDS.x, maxY - y0 )
      f.setUndecorated( true )
      f.setVisible( true )
      support.nuages = f

      Actor.actor {
         ProcTxn.atomic { implicit tx =>
            SoundProcesses.init( s )
         }
//         if( START_META ) ProcTxn.atomic { implicit tx =>
//            SemiNuages.meta.init
//         }
      }
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
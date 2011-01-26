/*
 *  ScalaInterpreterFrame.scala
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

import de.sciss.scalainterpreter.{ LogPane, ScalaInterpreterPane }
import de.sciss.synth.Server
import tools.nsc.Interpreter
import java.io.PrintStream
import de.sciss.synth.swing.NodeTreePanel
import java.awt.event.KeyEvent
import java.awt.{Toolkit, GraphicsEnvironment}
import InterPlay._
import javax.swing._

/**
 *    @version 0.11, 04-Jun-10
 */
class ScalaInterpreterFrame( support: REPLSupport /* s: Server, ntp: NodeTreePanel*/ )
extends JFrame( "Scala Interpreter" ) {
   val pane = new ScalaInterpreterPane
//   private val sync = new AnyRef
//   private var inCode: Option[ Interpreter => Unit ] = None
//   private var interpreter: Option[ Interpreter ] = None

   private val txnKeyStroke = {
      val ms = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
      KeyStroke.getKeyStroke( KeyEvent.VK_T, ms )
   }

   // ---- constructor ----
   {
      val cp = getContentPane

      pane.initialText = pane.initialText +
"""// Press '""" + KeyEvent.getKeyModifiersText( txnKeyStroke.getModifiers() ) + " + " +
      KeyEvent.getKeyText( txnKeyStroke.getKeyCode() ) + """' to execute transactionally.

Similarity.test( 7777, 43, normalize = true, tpe = Similarity.CC )

val b = SoundProcesses.anaClientBuf
val f = b.emptyFrame
val dmin = new Array[Double]( f.size )
val dmax = new Array[Double]( f.size )
b.getFrame( 0, f )
for( y <- 0 until b.numChannels ) {
    dmin(y)=f(y); dmax(y)=f(y)
}
for( x <- 1 until b.numFrames ) {
    b.getFrame( x, f )
    for( y <- 0 until b.numChannels ) {
        dmin(y)=math.min(dmin(y),f(y)); dmax(y)=math.max(dmax(y),f(y))
    }
}
// (0 until b.numChannels).find( i => dmin(i) >= dmax(i))

for( x <- 0 until b.numFrames ) {
    b.getFrame( x, f )
    for( y <- 0 until b.numChannels ) {
        f(y) = ((f(y) - dmin(y)) / (dmax(y) - dmin(y))).toFloat
    }
    b.setFrame( x, f )
}

//
for( x <- 0 until b.numFrames ) {
    b.getFrame( x, f )
    for( y <- 0 until b.numChannels ) {
        dmin(y)=math.min(dmin(y),f(y)); dmax(y)=math.max(dmax(y),f(y))
    }
}


//InterPlay.LIVE_FILE = Some( "live110124_190643.irc" )
SoundProcesses.playPath = Some( new java.io.File( SoundProcesses.livePath, "live110124_190643.irc" ))

Similarity.search( Similarity.templates( "string" ), 0.5f, 30, 1.0f ) { res => println( "SEARCH: " + res )}
//Similarity.saveTemplate( 11796, 86, "gliss" )
"""

      pane.initialCode = Some(
"""
import math._
import de.sciss.synth._
import de.sciss.synth.ugen._
import de.sciss.synth.swing._
import de.sciss.synth.proc._
import de.sciss.synth.proc.DSL._
import de.sciss.interplay._
import support._
"""
      )

      pane.bindingsCreator = Some( (in: Interpreter ) => {
//         sync.synchronized {
//            interpreter = Some( in )
//println( "bindingsCreator " + inCode.isDefined )
//            inCode.foreach( _.apply( in ))
//         }
         in.bind( "support", classOf[ REPLSupport ].getName, support )
//         in.bind( "ntp", classOf[ NodeTreePanel ].getName, ntp )
//         in.bind( "in", classOf[ Interpreter ].getName, in )
      })

//      val lp = new LogPane
//      lp.init
      pane.out = Some( logPane.writer )
//      Console.setOut( lp.outputStream )
//      Console.setErr( lp.outputStream )
//      System.setErr( new PrintStream( lp.outputStream ))

      pane.customKeyMapActions += txnKeyStroke -> (() => txnExecute)

      pane.init
//      val sp = new JSplitPane( SwingConstants.HORIZONTAL )
//      sp.setTopComponent( pane )
//      sp.setBottomComponent( lp )
//      cp.add( sp )
      cp.add( pane )
      val b = GraphicsEnvironment.getLocalGraphicsEnvironment.getMaximumWindowBounds
      setSize( b.width / 2, b.height * 7 / 8 )
//      sp.setDividerLocation( b.height * 2 / 3 )
      setLocationRelativeTo( null )
//    setLocation( x, getY )
      setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
//    setVisible( true )
   }

   private var txnCount = 0

   def txnExecute {
      pane.getSelectedTextOrCurrentLine.foreach( txt => {
         val txnId  = txnCount
         txnCount += 1
         val txnTxt = """class _txnBody""" + txnId + """( implicit t: ProcTxn ) {
""" + txt + """
}
val _txnRes""" + txnId + """ = ProcTxn.atomic( implicit t => new _txnBody""" + txnId + """ )
import _txnRes""" + txnId + """._
"""

//         println( txnTxt )
         pane.interpret( txnTxt )
      })
   }

//   def withInterpreter( fun: Interpreter => Unit ) {
//      sync.synchronized {
//println( "withInterpreter " + interpreter.isDefined )
//         interpreter.map( fun( _ )) getOrElse {
//            inCode = Some( fun )
//         }
//      }
//   }
}
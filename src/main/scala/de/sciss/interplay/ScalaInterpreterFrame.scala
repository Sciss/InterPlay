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

import java.awt.event.KeyEvent
import java.awt.{GraphicsEnvironment, Toolkit}
import javax.swing._

import de.sciss.interplay.InterPlay._
import de.sciss.scalainterpreter.{CodePane, Interpreter, InterpreterPane}

import scala.tools.nsc.interpreter.NamedParam

/**
 *    @version 0.11, 04-Jun-10
 */
class ScalaInterpreterFrame( support: REPLSupport /* s: Server, ntp: NodeTreePanel*/ )
extends JFrame( "Scala Interpreter" ) {
  private val txnKeyStroke = {
    val ms = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
    KeyStroke.getKeyStroke( KeyEvent.VK_T, ms )
  }

  val pConfig = InterpreterPane.Config()
  val cConfig = CodePane.Config()
  val iConfig = Interpreter.Config()

  cConfig.text =
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

  iConfig.bindings :+=
    NamedParam("support", support)

  cConfig.keyMap += txnKeyStroke -> (() => txnExecute)

  iConfig.out = Some(logPane.writer)

  iConfig.imports ++= Seq(
  "import math._", "de.sciss.synth._", "de.sciss.synth.ugen._", "de.sciss.synth.swing._", "de.sciss.synth.proc._",
    "de.sciss.synth.proc.DSL._", "de.sciss.interplay._", "support._")

  val pane = InterpreterPane(pConfig, iConfig, cConfig)
//   private val sync = new AnyRef
//   private var inCode: Option[ Interpreter => Unit ] = None
//   private var interpreter: Option[ Interpreter ] = None

   // ---- constructor ----
   {
      val cp = getContentPane

      cp.add( pane.component.peer )
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
      pane.codePane.activeText.foreach( txt => {
         val txnId  = txnCount
         txnCount += 1
         val txnTxt = """class _txnBody""" + txnId + """( implicit t: ProcTxn ) {
""" + txt + """
}
val _txnRes""" + txnId + """ = atomic( implicit t => new _txnBody""" + txnId + """ )
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
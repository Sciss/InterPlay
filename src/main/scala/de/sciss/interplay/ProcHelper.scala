/*
 *  ProcHelper.scala
 *  (Dissemination)
 *
 *  Copyright (c) 2010 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.synth.proc.{ProcEdge, DSL, ProcFilter, Proc, ProcTxn}
import DSL._
import java.io.File
//import Dissemination._

object ProcHelper {
   val verbose = false

//   def createTempAudioFile = File.createTempFile( "semi", ".aif", new File( TEMP_PATH ))

   def whenGlideDone( p: Proc, ctrlName: String )( fun: ProcTxn => Unit )( implicit tx: ProcTxn ) {
      if( p.control( ctrlName ).cv.mapping.isEmpty ) {
         fun( tx )
      } else {
         lazy val l: Proc.Listener = new Proc.Listener {
            def updated( u: Proc.Update ) {
               if( u.controls.find( tup => (tup._1.name == ctrlName) && tup._2.mapping.isEmpty ).isDefined ) {
                  ProcTxn.atomic { implicit tx =>
                     p.removeListener( l )
                     fun( tx )
                  }
               }
            }
         }
         p.addListener( l )
      }
   }

   def whenFadeDone( p: Proc )( fun: ProcTxn => Unit )( implicit tx: ProcTxn ) {
      if( !p.state.fading ) {
         fun( tx )
      } else {
         lazy val l: Proc.Listener = new Proc.Listener {
            def updated( u: Proc.Update ) {
               if( !u.state.fading ) {
                  ProcTxn.atomic { implicit tx =>
                     p.removeListener( l )
                     fun( tx )
                  }
               }
            }
         }
         p.addListener( l )
      }
   }

   private def stopAndDisposeListener( preFun: ProcTxn => Unit, postFun: ProcTxn => Unit ) = new Proc.Listener {
      def updated( u: Proc.Update ) {
//println( "UPDATE " + u )
         if( !u.state.fading && (u.state.bypassed || u.controls.find( tup =>
            (tup._1.name == "amp") && tup._2.mapping.isEmpty ).isDefined) ) {
            if( verbose ) println( "" + new java.util.Date() + " FINAL-DISPOSE " + u.proc )
            disposeProc( u.proc, preFun, postFun ) // ProcTxn.atomic { implicit tx => }
         }
      }
   }

   private def disposeProc( proc: Proc, preFun: ProcTxn => Unit, postFun: ProcTxn => Unit ) {
      ProcTxn.atomic { implicit tx =>
         preFun( tx )
         proc.anatomy match {
            case ProcFilter   => disposeFilter( proc )
            case _            => disposeGenDiff( proc )
         }
         postFun( tx )
      }
   }

   // XXX copied from Nuages. we should have this going into SoundProcesses directly somehow
   private def disposeFilter( proc: Proc )( implicit tx: ProcTxn ) {
      val in   = proc.audioInput( "in" )
      val out  = proc.audioOutput( "out" )
      val ines = in.edges.toSeq
      val outes= out.edges.toSeq
      val outesf = outes.filterNot( _.targetVertex.name.startsWith( "$" ))  // XXX tricky shit to determine the meters
//      if( ines.size > 1 ) println( "WARNING : Filter is connected to more than one input!" )
      if( ines.size > 1 && outesf.size > 1 ) {
         println( "WARNING : Filter is connected to several inputs and outputs! (" + proc.name + " : inputs = " +
         ines.map( _.sourceVertex ) + " ; outputs = " + outesf.map( _.targetVertex ) + ")" )
      }
      if( verbose && outes.nonEmpty ) println( "" + new java.util.Date() + " " + out + " ~/> " + outes.map( _.in ))
      outes.foreach( out ~/> _.in )
      ines.foreach( ine => {
         val out = ine.out
         if( verbose ) println( "" + new java.util.Date() + " " + out + " ~> " + outesf.map( _.in ))
         outesf.foreach( out ~> _.in )
      })
      // XXX tricky: this needs to be last, so that
      // the pred out's bus isn't set to physical out
      // (which is currently not undone by AudioBusImpl)
      if( verbose && ines.nonEmpty ) println( "" + new java.util.Date() + " " + ines.map( _.out ) + " ~/> " + in )
      ines.foreach( _.out ~/> in )
      proc.dispose
   }

   // XXX copied from Nuages. we should have this going into SoundProcesses directly somehow
   private def disposeGenDiff( p: Proc )( implicit tx: ProcTxn ) {
//      val toDispose = MSet.empty[ Proc ]
//      addToDisposal( toDispose, proc )
//      toDispose.foreach( p => {
//val p = proc
         val ines = p.audioInputs.flatMap( _.edges ).toSeq // XXX
         val outes= p.audioOutputs.flatMap( _.edges ).toSeq // XXX
         outes.foreach( oute => oute.out ~/> oute.in )
         ines.foreach( ine => ine.out ~/> ine.in )
         p.dispose
//      })
   }

   def stopAndDispose( p: Proc, fadeTime: Double = 0.0, preFun: ProcTxn => Unit = _ => (), postFun: ProcTxn => Unit = _ => () )( implicit tx: ProcTxn ) {
      val state = p.state
//println( "STOP-AND-DISPOSE " + p + " -> " + state + " / " + tx.transit )
//      if( !state.fading && (!state.playing || state.bypassed || (tx.transit == Instant)) ) {
      if( !state.fading && (!state.playing || state.bypassed || (fadeTime == 0.0)) ) {
//println( ".......INSTANT" )
//         preFun( tx )
//         p.dispose
//         postFun( tx )
         disposeProc( p, preFun, postFun )
      } else {
         p.addListener( stopAndDisposeListener( preFun, postFun ))
         p.anatomy match {
            case ProcFilter => {
//println( ".......BYPASS" )
               xfade( fadeTime ) { p.bypass }
            }
            case _ => {
//println( ".......STOP " + (new java.util.Date()) )
//               p.stop
               glide( fadeTime ) { p.control( "amp" ).v = 0.001 }
            }
         }
      }
   }

   def playNewDiff( p: Proc, fadeTime: Double = 0.0, postFun: ProcTxn => Unit = _ => () )( implicit tx: ProcTxn ) {
      lazy val pl: Proc.Listener = new Proc.Listener {
         def updated( u: Proc.Update ) {
            if( u.audioBusesConnected.find( b => (b.sourceVertex == p) && (b.out.name == "out") ).isDefined ) {
                // why we need spawn?? wolkenpumpe doesn't get the new state update otherwise. why?? XXX
               ProcTxn.spawnAtomic { implicit tx =>
                  p.removeListener( pl )
                  if( fadeTime > 0 ) xfade( fadeTime ) { p.play } else p.play
                  postFun( tx )
               }
            }
         }
      }
      p.addListener( pl )
   }

   def findOutEdge( src: Proc, tgt: Proc )( implicit tx: ProcTxn ) : Option[ ProcEdge ] = {
      src.outEdges find { e =>
         val v = e.targetVertex
         if( v == tgt ) true else findOutEdge( v, tgt ).isDefined
      }
   }
}
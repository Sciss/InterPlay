/*
 *  Midi.scala
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

import javax.sound.midi._
import de.sciss.nuages.NuagesPanel
import de.sciss.synth.proc.ProcTxn
import SoundProcesses._

object Midi {
   val IN_DESCR   = "BCF2000 Port 1"
   val OUT_DESCR  = "BCF2000 Port 1"

   var DUMP_IN    = false
   var DUMP_OUT   = false

   private def inform( what: String ) {
      println( "Midi : " + what )
   }

   private val sync = new AnyRef
   private var out: Receiver = _
   private val outMsg = new ShortMessage()

   def init( p: NuagesPanel ) {
      try {
         val infos   = MidiSystem.getMidiDeviceInfo()
         val inDevO  = infos.filter( _.getDescription == IN_DESCR  ).map( MidiSystem.getMidiDevice( _ )).find( _.getMaxTransmitters() != 0 )
         val outDevO = infos.filter( _.getDescription == OUT_DESCR ).map( MidiSystem.getMidiDevice( _ )).find( _.getMaxReceivers() != 0 )

         (inDevO, outDevO) match {
            case (Some( inDev ), Some( outDev )) =>
               inDev.open
               outDev.open
               val t = inDev.getTransmitter()
               t.setReceiver( new Receiver {
                  def close { inform( "Input closed" )}
                  def send( m: MidiMessage, time: Long ) {
                     m match {
                        case sm: ShortMessage if( sm.getCommand() == ShortMessage.CONTROL_CHANGE ) =>
                           val ch   = sm.getChannel()
                           val num  = sm.getData1()
                           val v    = sm.getData2()
                           if( DUMP_IN ) inform( "cc in,  ch " + ch + ", num " + num + ", v " + v )
                           if( num == 7 ) {           // faders
                              if( ch == 0 ) {         // mic
                                 ProcTxn.atomic { implicit tx =>
                                    if( pLiveDiff.state.valid ) {
                                       pLiveDiff.control( "amp" ).v = LIVE_AMP_SPEC._1.map( v.toDouble / 127 )
                                    }
                                 }
                              } else if( ch == 1 ) {  // main
                                 ProcTxn.atomic { implicit tx =>
                                    val vol = NuagesPanel.masterAmpSpec._1.map( v.toDouble / 127 )
//inform( "setVolume " + vol )
                                    p.setMasterVolume( vol )
                                 }
                              }
                           } else if( num == 89 ) {   // play
                              if( num > 0 ) {
                                 startLive
                              }
                           }

                        case _ =>
                     }
                  }
               })
               sync.synchronized { out = outDev.getReceiver() }
               initFaders( p )

            case _ =>
               if( inDevO.isEmpty )  inform( "No input device '" +  IN_DESCR +  "' found!" )
               if( outDevO.isEmpty ) inform( "No output device '" + OUT_DESCR + "' found!" )
         }
      } catch {
         case e =>
            inform( "Error initializing MIDI: ")
            e.printStackTrace()
      }
   }

   def initFaders( p: NuagesPanel ) {
      ccOut( 0, 7, (LIVE_AMP_SPEC._1.unmap( LIVE_AMP_SPEC._2 ) * 127 + 0.5).toInt )
      ccOut( 1, 7, (NuagesPanel.masterAmpSpec._1.unmap( NuagesPanel.masterAmpSpec._2 ) * 127 + 0.5).toInt )
      ccOut( 0, 89, 0 )
   }

   def ccOut( ch: Int, num: Int, v: Int ) {
      sync.synchronized {
         if( out == null ) return
         if( DUMP_OUT ) inform( "cc out, ch " + ch + ", num " + num + ", v " + v )
         outMsg.setMessage( ShortMessage.CONTROL_CHANGE, ch, num, v )
         out.send( outMsg, -1 )
      }
   }
}
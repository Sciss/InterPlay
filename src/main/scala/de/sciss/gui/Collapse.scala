/*
 *  Collapse.java
 *  (de.sciss.gui package)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package de.sciss.gui

import java.awt.event.{ActionListener, ActionEvent}
import javax.swing.Timer

object Collapse {
   def apply[ T ]( time: Double )( coll: (T, T) => T )( done: T => Unit ) =
      new Collapse( time, coll, done )
}
class Collapse[ T ]( val time: Double, coll: (T, T) => T, done: T => Unit ) {
   private val timer = {
      val t = new Timer( (time * 1000 + 0.5).toInt, new ActionListener {
         def actionPerformed( e: ActionEvent ) { perform() }
      })
      t.setRepeats( false )
      t
   }
   private var cancelled = false
   private var vo = Option.empty[ T ]
   private val sync = new AnyRef

   private def perform() {
      sync.synchronized {
         if( !cancelled ) {
            vo.foreach( done( _ ))
            vo = None
         }
      }
   }

   def apply( v: T ) {
      sync.synchronized {
//         require( !cancelled, "Collapse was cancelled" )
         cancelled = false
         vo = vo.map( coll( _, v )).orElse( Some( v ))
         timer.start
      }
   }

   def cancel() {
      sync.synchronized {
         cancelled = true
         timer.stop
         vo = None
      }
   }

//   def isCancelled : Boolean = sync.synchronized( cancelled )
}
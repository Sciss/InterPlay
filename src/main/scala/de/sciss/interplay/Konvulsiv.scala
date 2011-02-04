/*
 *  Konvulsiv.scala
 *  (InterPlay)
 *
 *  Copyright (c) 2010-2011 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.synth.proc.{Ref, ProcTxn}

object Konvulsiv {
   var verbose = true

   def konvul( name: String, time: (Double, Double), kon: (Int, Int) )( iter: (Double, Int) => Option[ ((Double, Double), (Int, Int)) ]) =
      new Konvulsiv( name, Ref( Util.rrand( time._1, time._2 )), Ref( Util.rrand( kon._1, kon._2 )), iter )
}

class Konvulsiv private( val name: String, private var timeRef: Ref[ Double ], private var konRef: Ref[ Int ],
                         iter: (Double, Int) => Option[ ((Double, Double), (Int, Int)) ]) {
   import Konvulsiv._

   private val lastDecided = Ref( 0.0 )

   def decide( implicit tx: ProcTxn ) : Option[ Int ] = {
      val t       = SoundProcesses.logicalTime()
      val lastT   = lastDecided.swap( t )
      val tim     = timeRef()
      if( lastT <= tim && t > tim ) {
         val res = konRef()
         iter( tim, res ) match {
            case Some( ((minTime, maxTime), (minKon, maxKon)) ) =>
               timeRef.set( Util.rrand( minTime, maxTime ))
               konRef.set( Util.rrand( minKon, maxKon ))
            case None => timeRef.set( 999.0 )
         }
         if( verbose ) println( "  konvul " + name + " at time " + t + " yields " + res )
         Some( res )
      } else None
   }

   def decideOrElse( k: Int )( implicit tx: ProcTxn ) : Int = decide.getOrElse( k )
}
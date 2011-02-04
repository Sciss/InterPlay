/*
 *  ProccHoeren.scala
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

import de.sciss.synth._
import proc._
import ugen._
import DSL._
import InterPlay._
import SoundProcesses._
import collection.immutable.IntMap
import Tendency._

/**
 * Listens to individual channels, and when they stay too quiet after a climax,
 * injects material from the live buffer around detected attacks. Tries to
 * sync with attacks on other channels.
 */
object ProcHoeren extends Process {
   import Process._

   val name    = "p-hoeren"
   val verbose = true

   private val anaName = name + "-ana"

//   lazy val MIN_WAIT      = (liveDur - 1) * 60
//   lazy val MAX_WAIT      = liveDur * 60

   val MIN_DUR       = 1.0
   val MAX_DUR       = 60.0

//   val STICKY_DUR    = 10.0
//   val REENTRY_DUR   = 5.0

   val MAX_GENDELAY  = 3.0
   private val TEND_GENDELAY  = tend( name + "gendelay", Lin, 0.0 -> (0.0, MAX_GENDELAY), 1.0 -> (0.0, MAX_GENDELAY), 2.0 -> (0.0, MAX_GENDELAY * 0.1) )
   private val TEND_THRESH    = tend( name + "thresh", Lin, 0.0 -> (12.0, 12.0), 1.0 -> (12.0, 12.0), 2.0 -> (9.0, 18.0) )
   private val TEND_SPEED     = tend( name + "speed", Exp, 0.0 -> (1.0, 1.0), 1.0 -> (1.0, 1.0), (2.0, (0.25, 1.0), 'sin) )
   private val TEND_STICKY    = tend( name + "sticky", Lin, 0.0 -> (10.0, 10.0), 1.0 -> (10.0, 10.0), 2.0 -> (7.0, 20.0) )
   private val TEND_REENTRY   = tend( name + "reentry", Lin, 0.0 -> (10.0, 10.0), 1.0 -> (10.0, 10.0), 2.0 -> (7.0, 10.0) )

   private val anaRef   = Ref( Map.empty[ Proc, IntMap[ Org ]])   // ana-proc to chan->org
   private val genRef   = Ref( Map.empty[ Proc, Org ])   // gen-proc to org

   private case class Org( ch: Int, ana: Proc, gen: Proc, diff: Proc )

   def init( implicit tx: ProcTxn ) {
      filter( anaName ) {
//         val ptup    = pControl( "up", ParamSpec( 0, 16 ), 6 )
         val ptdown  = pControl( "thresh", ParamSpec( 0, 16 ), 6 )
         val pdly    = pControl( "delay", ParamSpec( 1, 16, ExpWarp ), 3 )
         val pdur    = pScalar( "dur", ParamSpec( 5, 30, ExpWarp ), 5 )
         graph { in =>
//            val threshUp   = ptup.kr
            val threshDown = ptdown.kr
            val me         = Proc.local
            in.outputs.zipWithIndex.foreach { tup =>
               val (chSig, ch) = tup
               val loudness   = Loudness.kr( FFT( bufEmpty( 1024 ).id, chSig ))
               val lagged     = LagUD.kr( loudness, 0, 2 )
////lagged.poll( 1, label = "Loud" )
//               val schmidt    = Schmidt.kr( lagged, threshUp, threshDown )
//               val trig       = -HPZ1.kr( schmidt ) // goes >0 if schmidt goes from hi to lo

               val trig = TDelay.kr( lagged < threshDown, pdly.kr )

               trig.react { spawnAtomic( name + " ana dang" + ch ) { implicit tx =>
                  dang( me, ch )
               }}
            }
            Done.kr( Line.kr( 0, 0, pdur.ir )).react {
               spawnAtomic( name + " ana removal" ) { implicit tx =>
                  ProcessHelper.stopAndDispose( me )
                  anaRef.transform( _ - me )
                  reentry
               }
            }
            in // thru
         }
      }

      gen( name ) {
         val pdly    = pScalar( "dly", ParamSpec( 0, MAX_GENDELAY ), 0 )
         val ppos    = pScalar( "pos", ParamSpec( 0, 1 ), 0 )
         val pdur    = pScalar( "dur", ParamSpec( MIN_DUR, MAX_DUR ), MIN_DUR )
         val pspeed  = pScalar( "speed", ParamSpec( 1.0/8, 8, ExpWarp ), 1 )
         graph {
            val speed      = pspeed.ir
            val dur0       = pdur.ir
            val dur        = dur0 / speed
            val atk        = 0.011 // 0.02
//            val rls        = (dur - atk).min( 2 ).max( 0 )
            val dly        = pdly.ir
            val rls0       = dur - atk
            val sus        = (rls0 - 1).min( 0.5 ).max( 0 )
//            val rls        = (rls0 - rls0.min( 0.5 )) // .max( 0 )
            val rls        = rls0 - sus
//            val sus        = (dur - rls - atk).max( 0 )
            val freq       = speed / dur
            val sig        = PlayBuf.ar( liveBuf.numChannels, liveBuf.id, speed, startPos = ppos.ir * BufFrames.ir( liveBuf.id ), loop = 0 )
//            val env        = EnvGen.ar( Env.linen( atk, sus, rls, shape = cubShape ))

//DC.kr( dly ).poll( 0.1, label = "dly" )
//DC.kr( atk ).poll( 0.1, label = "atk" )
//DC.kr( sus ).poll( 0.1, label = "sus" )
//DC.kr( rls ).poll( 0.1, label = "rls" )

            val env        = EnvGen.ar( new Env( 0, List( EnvSeg( dly, 0 ), EnvSeg( atk, 1, sqrShape ), EnvSeg( sus, 1 ), EnvSeg( rls, 0, sqrShape ))))
            val me         = Proc.local
            Done.kr( env ).react {
               spawnAtomic( name + " gen done" ) { implicit tx =>
                  genRef().get( me ) match {
                     case Some( org ) =>
                        genRef.transform( _ - me )
                        anaRef.transform { anaMap =>
                           anaMap.get( org.ana ).map( intMap => {
                              val intMap1 = intMap - org.ch
                              if( intMap1.isEmpty ) {
                                 anaMap - org.ana
                              } else {
                                 anaMap + (org.ana -> intMap1)
                              }
                           }).getOrElse( anaMap )
                        }
                        Process.removeAndDispose( name + " gen all done", org.diff )

                     case None => inform( "Wooop. no org for gen-proc", force = true )
                  }
               }
            }
//val sig1 = WhiteNoise.ar( Seq.fill( sig.numOutputs )( 0.2 ))
            val cmp = Compander.ar( sig * env, sig, thresh = 0.5, ratioBelow = 1.0/3, ratioAbove = 1, attack = 0.01, release = 1.0 )   // compress low stuff
//            cmp * env
            cmp
//            WhiteNoise.ar( Seq.fill( cmp.numOutputs )( 0.2 )) * env
         }
      }

      delay( liveDur * 60 )( spawnAtomic( name + " delay done" )( start( _ )))
//delay( 20 )( atomic( start( _ )))
   }

   private def start( implicit tx: ProcTxn ) {
      if( canReplaceTail( ReplaceAll )) {
         val p = factory( anaName ).make
         p.control( "dur" ).v = TEND_STICKY.decide
         replaceTail( p, point = ReplaceAll )
      }
   }

   private def reentry( implicit tx: ProcTxn ) {
      delay( TEND_REENTRY.decide )( spawnAtomic( name + " reentry done" )( start( _ )))
   }

   private def dang( ana: Proc, ch: Int )( implicit tx: ProcTxn ) {
      if( anaRef().get( ana ).flatMap( _.get( ch )).isDefined ) return // no double dangs per channel

      inform( "dang " + ch )
      val ms   = anaMarkers.all.toIndexedSeq
      if( ms.isEmpty ) return
      val m    = Util.choose( ms )
      val mi   = ms.indexOf( m )
      val m0   = math.max( 0, m - 1 )
      val m2   = if( mi == ms.size - 1 ) anaClientBuf.framesWritten else math.max( m0, ms( mi + 1 ) /* - 1 */ )

//      val pos  = framesToPos( m0 )
      val dur  = math.min( MAX_DUR, math.max( MIN_DUR, frameToSecs( m2 ) - frameToSecs( m )))
      val secs0= frameToSecs( m0 )
      val dly  = math.min( secs0, Util.rand( TEND_GENDELAY.decide ))
      val pos  = secsToPos( secs0 - dly )

      val p    = factory( name ).make
      val d    = factory( "O-one" ).make
      p.control( "dly" ).v = dly
      p.control( "pos" ).v = pos
      p.control( "dur" ).v = dur
      p.control( "speed" ).v = TEND_SPEED.decide
      d.control( "idx" ).v = ch
      p ~> d
      addTail( d )

      val org = Org( ch, ana, p, d )
      anaRef.transform( map => map + (ana -> (map.getOrElse( ana, IntMap.empty ) + (ch -> org))))
      genRef.transform( _ + (p -> org) )
   }
}
package de.sciss.interplay

import de.sciss.synth._
import proc._
import ugen._
import DSL._
import InterPlay._
import SoundProcesses._

/**
 * Listens to individual channels, and when they stay too quiet after a climax,
 * injects material from the live buffer around detected attacks. Tries to
 * sync with attacks on other channels.
 */
object ProcHoeren extends Process {
   import Process._

   val name    = "p-hoeren"
   val verbose = false

   def init(  implicit tx: ProcTxn ) {
      filter( name ) {
         val ptup    = pControl( "up", ParamSpec( 0, 64 ), 32 )
         val ptdown  = pControl( "down", ParamSpec( 0, 64 ), 32 )
         graph { in =>
            val threshUp = ptup.kr
            val threshDown = ptdown.kr
            in.outputs.zipWithIndex.foreach { tup =>
               val (chSig, ch) = tup
               val loudness   = Loudness.kr( FFT( bufEmpty( 1024 ).id, chSig ))
               val lagged     = LagUD.kr( loudness, 0, 2 )
               val schmidt    = Schmidt.kr( lagged, threshUp, threshDown )
               val trig       = -HPZ1.kr( schmidt ) // goes >0 if schmidt goes from hi to lo
               trig.react {
                  // XXX todo
               }
            }
            in // thru
         }
      }
   }
}
package de.sciss.interplay

import de.sciss.synth.proc.ProcTxn

object ProcRiechen extends Process {
   val name    = "p-riech"
   val verbose = false

   def init(  implicit tx: ProcTxn ) {}
}
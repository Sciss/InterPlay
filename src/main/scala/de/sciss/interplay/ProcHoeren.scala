package de.sciss.interplay

import de.sciss.synth.proc.ProcTxn

object ProcHoeren extends Process {
   val name    = "p-hoeren"
   val verbose = false

   def init(  implicit tx: ProcTxn ) {}
}
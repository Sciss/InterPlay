package de.sciss.interplay

import de.sciss.synth.proc.ProcTxn

object ProcSehen extends Process {
   val name    = "p-seh"
   val verbose = false

   def init(  implicit tx: ProcTxn ) {}
}
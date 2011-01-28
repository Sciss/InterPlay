package de.sciss.interplay

import de.sciss.synth.proc.ProcTxn

object ProcTasten extends Process {
   val name    = "p-tast"
   val verbose = false

   def init(  implicit tx: ProcTxn ) {}
   def start( implicit tx: ProcTxn ) {}
   def stop(  implicit tx: ProcTxn ) {}
}
package de.sciss.interplay

import de.sciss.synth.proc.ProcTxn

object ProcOrientieren extends Process {
   val name    = "p-orient"
   val verbose = false

   def init(  implicit tx: ProcTxn ) {}
   def start( implicit tx: ProcTxn ) {}
   def stop(  implicit tx: ProcTxn ) {}
}
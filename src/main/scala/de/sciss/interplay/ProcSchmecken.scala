package de.sciss.interplay

import de.sciss.synth.proc.ProcTxn

object ProcSchmecken extends Process {
   val name    = "p-schmeck"
   val verbose = false

   def init(  implicit tx: ProcTxn ) {}
   def start( implicit tx: ProcTxn ) {}
   def stop(  implicit tx: ProcTxn ) {}
}
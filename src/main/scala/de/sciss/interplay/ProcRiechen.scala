package de.sciss.interplay

import de.sciss.synth._
import proc._
import ugen._
import InterPlay._
import SoundProcesses._
import DSL._
import Util._
import collection.immutable.{IndexedSeq => IIdxSeq}

object ProcRiechen extends Process {
   import Process._

   val name                = "p-riech"
   val verbose             = true

//   val MIN_WAIT            = 60.0
//   val MAX_WAIT            = 120.0
val MIN_WAIT            = 10.0
val MAX_WAIT            = 10.0

//   val LIVE_PROB           = 0.175
val LIVE_PROB           = 1.0 // force it
   val INT_PROB            = 0.75

   private val catchName   = name + "-catch"
   private val throwName   = name + "-throw"
//   private val freezeName  = name + "-freeze"
   private val hilbName    = name + "-hilb"
   private lazy val urn    = new Urn( hilbName )

   def init(  implicit tx: ProcTxn ) {
      diff( catchName ) {
         val pouts = Seq.tabulate( MASTER_NUMCHANNELS )( i => pAudioOut( "out" + i ))
         graph { in =>
            require( in.numOutputs == MASTER_NUMCHANNELS, "Just got " + in.numOutputs + " chans" )
            pouts.zip( in.outputs ).foreach { tup =>
               val (pout, sig) = tup
               pout.ar( sig ) // splittendorfer
            }
            0.0
         }
      }

      gen( throwName ) {
         val pins = Seq.tabulate( MASTER_NUMCHANNELS )( i => pAudioIn( "in" + i ))
         graph {
            val in: GE = pins.map( _.ar ) // klebendorfer
            require( in.numOutputs == MASTER_NUMCHANNELS )
            in
         }
      }

      filter( hilbName ) {
         val pin2  = pAudioIn( "in2" )
         graph { in1 =>
            val in2  = pin2.ar
            val norm = Normalizer.ar( in2, dur = 0.02 )
//            val norm = in2 * 2 * PeakFollower.ar( in2, 2 ).max( 0.01 ).reciprocal   // hmmm....
//            val rms  = LagUD.ar( in2.squared.squared, 0.1, 1.0 )
//            val norm = in2 * rms.max( 0.01 ).reciprocal
            val Seq(re1, im1) = Hilbert.ar( in1 ).outputs
            val Seq(re2, im2) = Hilbert.ar( norm ).outputs
            re1 * re2 - im1 * im2
         }
      }

      val dlyTime = rrand( MIN_WAIT, MAX_WAIT )
      inform( "waiting " + dlyTime )
      delay( dlyTime )( perform )
   }

   private def perform {
      ProcTxn.atomic { implicit tx =>
         inform( "perform" )
         startPlaying
         val pt = if( liveActive ) {
            val rnd = rand( 1.0 )
            if( rnd <= LIVE_PROB ) ReplaceLive else if( rnd - LIVE_PROB <= INT_PROB ) ReplaceInternal else ReplaceAll
         } else ReplaceInternal
         if( canReplaceTail( pt )) {
            val pin  = factory( catchName ).make
            val pout = factory( throwName ).make
            val modChans: IIdxSeq[ Int ] = scramble2( 0 until MASTER_NUMCHANNELS )
            val pflt = List.tabulate( MASTER_NUMCHANNELS ) { ch =>
               val res = factory( urn.next ).make
               pin.audioOutput( "out" + ch ) ~> res
               pin.audioOutput( "out" + modChans( ch )) ~> res.audioInput( "in2" )
               res ~> pout.audioInput( "in" + ch )
               res
            }
            val fdts = pflt.map( _ => exprand( 1.0, 20.0 ))
            replaceTailChain( pin, pout, pflt, fadeTimes = Some( fdts ), point = pt )
         }
      }
   }
}
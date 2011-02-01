package de.sciss.interplay

import de.sciss.synth._
import proc._
import ugen._
import InterPlay._
import SoundProcesses._
import DSL._
import Util._
import collection.immutable.{IndexedSeq => IIdxSeq, Set => ISet}

object ProcRiechen extends Process {
   import Process._

   val name                = "p-riech"
   val verbose             = true

//   val MIN_WAIT            = 60.0
//   val MAX_WAIT            = 120.0
   val MIN_WAIT            = 120.0
   val MAX_WAIT            = 180.0
//val MIN_WAIT            = 10.0
//val MAX_WAIT            = 10.0

   val LIVE_PROB           = 0.175
//val LIVE_PROB           = 1.0 // force it
   val INT_PROB            = 0.75

   val MIN_REENTRY         = 60.0
   val MAX_REENTRY         = 120.0
//val MIN_REENTRY         = 10.0
//val MAX_REENTRY         = 10.0

   val MIN_FADEIN          = 1.0
   val MAX_FADEIN          = 20.0

   val MIN_DUR             = 20.0
   val MAX_DUR             = 30.0

   val MIN_FADEOUT         = 1.0
   val MAX_FADEOUT         = 20.0

   private val catchName   = name + "-catch"
   private val throwName   = name + "-throw"
//   private val freezeName  = name + "-freeze"
   private val hilbName    = name + "-hilb"
   private lazy val urn    = new Urn( hilbName )

//   private val fltProcRef  = Ref( Set.empty[ Proc ])
   private val orgRef      = Ref( Option.empty[ Org ])
   case class Org( catchP: Proc, throwP: Proc, fltP: ISet[ Proc ])

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
         val pamp  = pAudio( "amp", ParamSpec( 0, 10 ), 1 )
         graph {
            val in: GE = pins.map( _.ar ) // klebendorfer
            require( in.numOutputs == MASTER_NUMCHANNELS )
            in * pamp.ar
         }
      }

      def mkDurMix() = pScalar( "dur", ParamSpec( 1, 60 ), 10 ) -> pAudio( "mix", ParamSpec( 0, 1 ), 1 )
      def mix( in: GE, flt: GE, pDurMix: (ProcParamScalar, ProcParamAudio) ) : GE = {
         val (pdur, pmix) = pDurMix
         val me = Proc.local
         Done.kr( Line.kr( 0, 0, pdur.ir )).react {
            ProcTxn.spawnAtomic { implicit tx =>
// unfortunately currently no way to determine end of this xfade...
//             xfade( ... )( me.bypass )
//               ProcessHelper.whenFadeDone()
               glide( exprand( MIN_FADEOUT, MAX_FADEOUT ))( me.control( "mix" ).v = 0 )
               ProcessHelper.whenGlideDone( me, "mix" ) { implicit tx =>
                  orgRef() match {
                     case Some( orgOld ) =>
                        val orgNew = orgOld.copy( fltP = orgOld.fltP - me )
                        if( orgNew.fltP.isEmpty ) {  // this was the last one
                           orgRef.set( None )
                           removeAndDisposeChain( orgNew.catchP, orgNew.throwP )
                           reentry()
                        } else {
                           orgRef.set( Some( orgNew ))
                        }
                     case None => informDir( "Weird - no org found?!", force = true )
                  }
               }
            }
         }
         XFade2.ar( in, flt, pmix.ar * 2 - 1 )
      }

      filter( hilbName ) {
         val pin2 = pAudioIn( "in2" )
         val dm   = mkDurMix()
         graph { in1 =>
            val in2  = pin2.ar
            val norm = Normalizer.ar( in2, dur = 0.02 )
//            val norm = in2 * 2 * PeakFollower.ar( in2, 2 ).max( 0.01 ).reciprocal   // hmmm....
//            val rms  = LagUD.ar( in2.squared.squared, 0.1, 1.0 )
//            val norm = in2 * rms.max( 0.01 ).reciprocal
            val Seq(re1, im1) = Hilbert.ar( in1 ).outputs
            val Seq(re2, im2) = Hilbert.ar( norm ).outputs
            val flt  = re1 * re2 - im1 * im2
            mix( in1, flt, dm )
         }
      }

      val dlyTime = rrand( MIN_WAIT, MAX_WAIT )
      inform( "waiting " + dlyTime )
      start( dlyTime )
   }

   private def reentry( factor: Double = 1.0 )( implicit tx: ProcTxn ) {
      inform( "Re-entry" )
      start( exprand( MIN_REENTRY, MAX_REENTRY ) * factor )
   }

   private def start( dlyTime: Double )( implicit tx: ProcTxn ) {
      delay( dlyTime )( perform )
   }

   private def perform {
      ProcTxn.atomic { implicit tx =>
         if( orgRef().isDefined ) {
            inform( "can't perform - old performance ongoing!", force = true )
            return // reentry??
         }
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
               res.control( "dur" ).v = exprand( MIN_DUR, MAX_DUR )
               pin.audioOutput( "out" + ch ) ~> res
               pin.audioOutput( "out" + modChans( ch )) ~> res.audioInput( "in2" )
               res ~> pout.audioInput( "in" + ch )
               res
            }
            val fdts = pflt.map( _ => exprand( MIN_FADEIN, MAX_FADEIN ))
            orgRef.set( Some( Org( pin, pout, pflt.toSet )))
            replaceTailChain( pin, pout, pflt, fadeTimes = Some( fdts ), point = pt )
         }
      }
   }
}
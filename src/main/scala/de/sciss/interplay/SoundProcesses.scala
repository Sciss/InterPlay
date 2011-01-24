package de.sciss.interplay

import de.sciss.synth._
import collection.immutable.{IndexedSeq => IIdxSeq}
import proc._
import ugen._
import DSL._
import InterPlay._

object SoundProcesses {
   val MAX_LIVE_DUR  = 10.0 * 60
   var liveBuf: Buffer = _
   var liveBufPhaseBus: AudioBus = _
   val MIC_CHANINDEX = 0
   val MIC_NUMCHANS  = 1

   var synPostM: Synth = _

   def init( s: Server )( implicit tx: ProcTxn ) {
      val bufFrames     = ((s.sampleRate * MAX_LIVE_DUR) + 0.5).toInt
      liveBuf           = Buffer.alloc( s, bufFrames, MIC_NUMCHANS )
      liveBufPhaseBus   = Bus.audio( s, 1 )

      gen( "live" ) {
         val pmode = pScalar( "mode", ParamSpec( 0, 1, LinWarp, 1 ), 0 )
         graph {
            val in: GE  = if( pmode.v == 0 ) {
               In.ar( NumOutputBuses.ir + MIC_CHANINDEX, MIC_NUMCHANS )
            } else {
               val path = "/Users/hhrutz/Desktop/InterPlay/rec/StringsDirect/StringsDirect1to4.aif"
               val disk = DiskIn.ar( 2, bufCue( path ).id, loop = 1 )
               IIdxSeq.tabulate( MIC_NUMCHANS )( i => disk \ (i % disk.numOutputs) )
            }
            val phase   = DelTapWr.ar( liveBuf.id, in )
            Out.ar( liveBufPhaseBus.index, phase )
            in
         }
      }

      gen( "live-dly" ) {
//         val pdly  = pAudio( "dly", ParamSpec( 0.0, MAX_LIVE_DUR ), 0.0 )
         val pdly  = pAudio( "dly", ParamSpec( MAX_LIVE_DUR / 1000, MAX_LIVE_DUR, ExpWarp ), MAX_LIVE_DUR / 1000 )
         graph {
            val phase   = InFeedback.ar( liveBufPhaseBus.index )
            val in      = DelTapRd.ar( liveBuf.id, phase, pdly.ar, 4 )
            in
         }
      }

//filter( "onsets" ) {
//    val pthresh = pControl( "thresh", ParamSpec( 0, 1 ), 0.2 )
//    val pdecay  = pControl( "decay", ParamSpec( 0.1, 10, ExpWarp ), 1 )
//    graph { in =>
//       val on = Onsets.kr( FFT( bufEmpty( 512 ).id, Mix( in )), pthresh.kr )
//       Decay.ar( Trig.ar( on, SampleDur.ir ), pdecay.kr )
//    }
//}

      diff( "O-mute" ) {
          graph { in =>
             val gagaism: GE = 0
             gagaism
          }
      }

      val chanConfigs = (("", 0, masterBus.numChannels) :: Nil) // (/*if( INTERNAL_AUDIO ) Nil else */ MASTER_CHANGROUPS))
      chanConfigs.zipWithIndex.foreach { tup =>
         val ((suff, chanOff, numCh), idx) = tup

         def placeChannels( sig: GE ) : GE = {
            if( numCh == masterBus.numChannels ) sig else {
               Vector.fill( chanOff )( Constant( 0 )) ++ sig.outputs ++ Vector.fill( masterBus.numChannels - (numCh + chanOff) )( Constant( 0 ))
            }
         }

         diff( "O-all" + suff ) {
             val pamp  = pAudio( "amp", ParamSpec( 0.01, 10, ExpWarp ), 1 )
             val pout  = pAudioOut( "out", None )

             graph { in =>
                val sig          = (in * Lag.ar( pamp.ar, 0.1 )).outputs
                val inChannels   = sig.size
                val outChannels  = numCh
                val outSig       = IIdxSeq.tabulate( outChannels )( ch => sig( ch % inChannels ))
                pout.ar( placeChannels( outSig ))
             }
         }
      }

      val dfPostM = SynthDef( "post-master" ) {
         val sig = In.ar( masterBus.index, masterBus.numChannels )
         // externe recorder
//         REC_CHANGROUPS.foreach { group =>
//            val (name, off, numOut) = group
//            val numIn   = masterBus.numChannels
//            val sig1: GE = if( numOut == numIn ) {
//               sig
//            } else if( numIn == 1 ) {
//               Vector.fill[ GE ]( numOut )( sig )
//            } else {
//               val sigOut  = Array.fill[ GE ]( numOut )( 0.0f )
//               val sca     = (numOut - 1).toFloat / (numIn - 1)
//               sig.outputs.zipWithIndex.foreach { tup =>
//                  val (sigIn, inCh) = tup
//                  val outCh         = inCh * sca
//                  val fr            = outCh % 1f
//                  val outChI        = outCh.toInt
//                  if( fr == 0f ) {
//                     sigOut( outChI ) += sigIn
//                  } else {
//                     sigOut( outChI )     += sigIn * (1 - fr).sqrt
//                     sigOut( outChI + 1 ) += sigIn * fr.sqrt
//                  }
//               }
//               Limiter.ar( sigOut.toSeq, (-0.2).dbamp )
//            }
//            assert( sig1.numOutputs == numOut )
//            Out.ar( off, sig1 )
//         }
         // master + people meters
         val meterTr    = Impulse.kr( 20 )
         //val trigA    = Trig1.ar( meterTr, SampleDur.ir )
         val (peoplePeak, peopleRMS) = {

val PEOPLE_CHANGROUPS = List.empty[ (String, Int, Int) ]

            val res = PEOPLE_CHANGROUPS.map { group =>
               val (_, off, numIn)  = group
               val pSig       = In.ar( NumOutputBuses.ir + off, numIn )
               val peak       = Peak.kr( pSig, meterTr ).outputs
               val peakM      = peak.tail.foldLeft[ GE ]( peak.head )( _ max _ ) \ 0
               val rms        = A2K.kr( Lag.ar( pSig.squared, 0.1 ))
               val rmsM       = (Mix( rms ) / numIn) \ 0
               (peakM, rmsM)
//               (Constant( 0 ), Constant( 0 ))
            }
            res.map( _._1 ) -> res.map( _._2 )  // elegant it's not
         }
         val masterPeak = Peak.kr( sig, meterTr )
         val masterRMS  = A2K.kr( Lag.ar( sig.squared, 0.1 ))
         val peak       = masterPeak.outputs ++ peoplePeak
         val rms        = masterRMS.outputs  ++ peopleRMS
         val meterData  = (peak zip rms).flatMap( tup => tup._1 :: tup._2 :: Nil )
         SendReply.kr( meterTr,  meterData, "/meters" )
      }
      synPostM = dfPostM.play( s, addAction = addToTail )
   }
}
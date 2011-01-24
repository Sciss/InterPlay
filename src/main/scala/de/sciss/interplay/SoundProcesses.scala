/*
 *  SoundProcesses.scala
 *  (InterPlay)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.interplay

import de.sciss.synth._
import collection.immutable.{IndexedSeq => IIdxSeq, SortedSet => ISortedSet}
import io.{AudioFile, AudioFileType}
import proc._
import ugen._
import DSL._
import InterPlay._
import java.nio.ByteBuffer
import java.io.{File, FileFilter}
import java.text.SimpleDateFormat
import java.util.Locale

object SoundProcesses {
   val diskBufSize   = 32768
   val anaFFTSize    = 1024
   val anaFFTOver    = 2
   val anaWinStep    = anaFFTSize / anaFFTOver

   val liveDur = 1.0   // minutes

   val maxLiveAnaFr   = {
      val secs = liveDur * 60
      val fr0  = (secs * 44100 + 0.5).toLong
      val f1   = fr0 - (fr0 % diskBufSize )
      assert( f1 % anaWinStep == 0 )
      (f1 / anaWinStep).toInt
   }
   val numMelCoeffs     = 13
   val anaChans         = numMelCoeffs
   val maxLiveFrames    = maxLiveAnaFr * anaWinStep
   val maxLiveDur       = maxLiveFrames / 44100.0 // 10.0 * 60
   var liveBuf: Buffer  = _
   var liveBufPhaseBus: AudioBus = _
//   val micChanIndex     = 0
//   val micNumChans      = 1

   var synPostM: Synth = _
//   val anaClientBuf    = ByteBuffer.allocateDirect( maxLiveAnaFr * anaChans * 4 ).asFloatBuffer
   val anaClientBuf    = new AnalysisBuffer( maxLiveAnaFr, anaChans ) // .asFloatBuffer
   val anaMarkers      = new AnalysisMarkers // ISortedSet.empty[ Int ]

   def init( s: Server )( implicit tx: ProcTxn ) {
      liveBuf           = Buffer.alloc( s, maxLiveFrames, MIC_NUMCHANNELS )
      liveBufPhaseBus   = Bus.audio( s, 1 )

      // -------------- GENS --------------



      gen( "live" ) {
         val pmode      = pScalar( "mode", ParamSpec( 0, 1, LinWarp, 1 ), 0 )
         val ppos       = pScalar( "pos", ParamSpec( 0, 1 ), 0 )
         val pthresh    = pControl( "thresh", ParamSpec( 0, 1 ), 0.2 )
         val cntUpd     = math.max( 1, (maxLiveAnaFr / maxLiveDur).toInt )
         val livePath   = new File( REC_PATH, "live" )
         var playPath   = livePath.listFiles( new FileFilter {
            def accept( p: File ) = {
               try {
                  val spec = AudioFile.readSpec( p )
                  (spec.numFrames > 0L) && (spec.numChannels == MIC_NUMCHANNELS)
               } catch {
                  case _ => false
               }
            }
         }).toList.sortBy( _.getName ).lastOption
         val df = new SimpleDateFormat( "'live'yyMMdd'_'HHmmss'.irc'", Locale.US )

         graph {
            val in: GE  = if( pmode.v == 0 ) {
               val res     = In.ar( NumOutputBuses.ir + MIC_OFFSET, MIC_NUMCHANNELS )
//               val recPath = File.createTempFile( "live", ".irc", livePath )
               val recPath = new File( livePath, df.format( new java.util.Date() ))
               playPath    = Some( recPath )
               DiskOut.ar( bufRecord( recPath.getAbsolutePath, MIC_NUMCHANNELS, AudioFileType.IRCAM ).id, res )
               res
            } else {
               val path = playPath.getOrElse( error( "No live recordings" )) // /Users/hhrutz/Desktop/InterPlay/rec/StringsDirect/StringsDirect1to4.aif" )
               val disk = DiskIn.ar( MIC_NUMCHANNELS, bufCue( path.getAbsolutePath ).id, loop = 1 )
//               IIdxSeq.tabulate( MIC_NUMCHANNELS )( i => disk \ (i % disk.numOutputs) )
               disk
            }
            val phase   = DelTapWr.ar( liveBuf.id, in )
            Out.ar( liveBufPhaseBus.index, phase )
            val chain   = FFT( bufEmpty( anaFFTSize ).id, in, anaFFTOver.reciprocal )
            val coeffs  = MFCC.kr( chain, numMelCoeffs )
            val onsets  = Onsets.kr( chain, pthresh.kr )
            val fftTrig = Impulse.kr( SampleRate.ir / anaWinStep )
            val fftCnt  = PulseCount.kr( fftTrig )

//            SendReply.kr( fftTrig, coeffs, "/ana" )
            val me = Proc.local
            fftTrig.react( fftCnt +: onsets +: coeffs.outputs ) { data =>
               val floats: Array[ Float ] = data.drop( 2 ).map( _.toFloat )( collection.breakOut )
               val cnt     = data( 0 ).toInt - 1
               val onset   = data( 1 ) > 0
               if( cnt < maxLiveAnaFr ) {
//println( "position " + (cnt * anaChans) + " ; floats.size " + floats.size )
//                  anaClientBuf.position( cnt * anaChans )
//                  anaClientBuf.put( floats )
                  anaClientBuf.setFrame( cnt, floats )
                  if( onset ) anaMarkers.add( cnt )
                  if( cnt % cntUpd == 0 ) ProcTxn.atomic { implicit tx =>
                     me.control( "pos" ).v = cnt.toDouble / maxLiveAnaFr
                  }
               } else {
                  ProcTxn.atomic { implicit tx =>
                     me.stop
                     me.control( "pos" ).v = 1.0
                  }
               }
            }

//            PauseSelf.kr( fftCnt === maxLiveAnaFr )
            PauseSelf.kr( fftCnt > maxLiveAnaFr )

            in
         }
      }

      gen( "live-dly" ) {
//         val pdly  = pAudio( "dly", ParamSpec( 0.0, maxLiveDur ), 0.0 )
         val pdly  = pAudio( "dly", ParamSpec( maxLiveDur / 1000, maxLiveDur, ExpWarp ), maxLiveDur / 1000 )
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


      // -------------- FILTERS --------------

      def mix( in: GE, flt: GE, mix: ProcParamAudio ) : GE = LinXFade2.ar( in, flt, mix.ar * 2 - 1 )
      def pMix = pAudio( "mix", ParamSpec( 0, 1 ), 1 )

      filter( "delay" ) {
         val ptime   = pAudio( "time", ParamSpec( 0.3,  30.0, ExpWarp ), 10 )
         val pfeed   = pAudio( "feed", ParamSpec( 0.001, 1.0, ExpWarp ), 0.001 )
         val pmix    = pMix
         graph { in =>
            val numFrames  = (sampleRate * 30).toInt
            val numChannels= in.numOutputs
            val buf        = bufEmpty( numFrames, numChannels )
            val bufID      = buf.id
            val time       = ptime.ar
            val lin        = LocalIn.ar( numChannels )
            val feed       = pfeed.ar
            val wDry       = (1 - feed).sqrt
            val wWet       = feed.sqrt
//            val phase      = DelTapWr.ar( bufID, (in * wDry) + (lin * wWet) )
//            val flt0       = DelTapRd.ar( bufID, phase, time, 2 ) // interp: 1 none, 2 linear 4 cubic
            val flt0       = BufDelayL.ar( bufID, (in * wDry) + (lin * wWet), time )
            val flt        = LeakDC.ar( flt0 )
            LocalOut.ar( flt )

            mix( in, flt, pmix )
         }
      }

      filter( "mantissa" ) {
         val pbits   = pAudio( "bits", ParamSpec( 2, 14, LinWarp, 1 ), 14 )
         val pmix    = pMix

         graph { in =>
            val flt  = MantissaMask.ar( in, pbits.ar )
            mix( in, flt, pmix )
         }
      }

      filter( "achil") {
         val pspeed  = pAudio( "speed", ParamSpec( 0.125, 2.3511, ExpWarp ), 0.5 )
         val pmix    = pMix

         graph { in =>
            val speed	   = Lag.ar( pspeed.ar, 0.1 )
            val numFrames  = sampleRate.toInt
            val numChannels= in.numOutputs
            val buf        = bufEmpty( numFrames, numChannels )
            val bufID      = buf.id
            val writeRate  = BufRateScale.kr( bufID )
            val readRate   = writeRate * speed
            val readPhasor = Phasor.ar( 0, readRate, 0, numFrames )
            val read			= BufRd.ar( numChannels, bufID, readPhasor, 0, 4 )
            val writePhasor= Phasor.ar( 0, writeRate, 0, numFrames )
            val old			= BufRd.ar( numChannels, bufID, writePhasor, 0, 1 )
            val wet0 		= SinOsc.ar( 0, ((readPhasor - writePhasor).abs / numFrames * math.Pi) )
            val dry			= 1 - wet0.squared
            val wet			= 1 - (1 - wet0).squared
            BufWr.ar( (old * dry) + (in * wet), bufID, writePhasor )
            mix( in, read, pmix )
         }
      }

      filter( "a-gate" ) {
         val pamt = pAudio( "amt", ParamSpec( 0, 1 ), 1 )
         val pmix = pMix
         graph { in =>
            val amount = Lag.ar( pamt.ar, 0.1 )
            val flt = Compander.ar( in, in, Amplitude.ar( in * (1 - amount ) * 5 ), 20, 1, 0.01, 0.001 )
            mix( in, flt, pmix )
         }
      }

      filter( "a-hilb" ) {
         val pmix = pMix
         graph { in =>
            var flt: GE = List.fill( in.numOutputs )( 0.0 )
            in.outputs foreach { ch =>
               val hlb  = Hilbert.ar( DelayN.ar( ch, 0.01, 0.01 ))
               val hlb2 = Hilbert.ar( Normalizer.ar( ch, dur = 0.02 ))
               flt     += (hlb \ 0) * (hlb2 \ 0) - (hlb \ 1 * hlb2 \ 1)
            }
            mix( in, flt, pmix )
         }
      }

      filter( "hilbert" ) {
         val pfreq   = pAudio( "freq", ParamSpec( -1, 1 ), 0.0 )
         val pmix    = pMix
         graph { in =>
            val freq    = pfreq.ar
            val freqHz  = freq.abs.linexp( 0, 1, 20, 12000 ) * freq.signum
            val flt     = FreqShift.ar( in, freqHz )
            mix( in, flt, pmix )
         }
      }

      filter( "filt" ) {
         val pfreq   = pAudio( "freq", ParamSpec( -1, 1 ), 0.54 )
         val pmix    = pMix

         graph { in =>
            val normFreq	= pfreq.ar
            val lowFreqN	= Lag.ar( Clip.ar( normFreq, -1, 0 ))
            val highFreqN	= Lag.ar( Clip.ar( normFreq,  0, 1 ))
            val lowFreq		= LinExp.ar( lowFreqN, -1, 0, 30, 20000 )
            val highFreq	= LinExp.ar( highFreqN, 0, 1, 30, 20000 )
            val lowMix		= Clip.ar( lowFreqN * -10.0, 0, 1 )
            val highMix		= Clip.ar( highFreqN * 10.0, 0, 1 )
            val dryMix		= 1 - (lowMix + highMix)
            val lpf			= LPF.ar( in, lowFreq ) * lowMix
            val hpf			= HPF.ar( in, highFreq ) * highMix
            val dry			= in * dryMix
            val flt			= dry + lpf + hpf
            mix( in, flt, pmix )
         }
      }

      filter( "frgmnt" ) {
   		val pspeed  = pAudio(   "speed", ParamSpec( 0.125, 2.3511, ExpWarp ), 1 )
		   val pgrain  = pControl( "grain", ParamSpec( 0, 1 ), 0.5 )
		   val pfeed   = pAudio(   "fb",    ParamSpec( 0, 1 ), 0 )
         val pmix    = pMix

         graph { in =>
            val bufDur        = 4.0
            val numFrames     = (bufDur * sampleRate).toInt
            val numChannels   = in.numOutputs
            val buf           = bufEmpty( numFrames, numChannels )
            val bufID         = buf.id

            val feedBack	   = Lag.ar( pfeed.ar, 0.1 )
            val grain	      = pgrain.kr // Lag.kr( grainAttr.kr, 0.1 )
            val maxDur	      = LinExp.kr( grain, 0, 0.5, 0.01, 1.0 )
            val minDur	      = LinExp.kr( grain, 0.5, 1, 0.01, 1.0 )
            val fade		      = LinExp.kr( grain, 0, 1, 0.25, 4 )
            val rec		      = (1 - feedBack).sqrt
            val pre		      = feedBack.sqrt
            val trig		      = LocalIn.kr( 1 )
            val white	      = TRand.kr( 0, 1, trig )
            val dur		      = LinExp.kr( white, 0, 1, minDur, maxDur )
            val off0		      = numFrames * white
            val off		      = off0 - (off0 % 1.0)
            val gate		      = trig
            val lFade	      = Latch.kr( fade, trig )
            val fadeIn	      = lFade * 0.05
            val fadeOut	      = lFade * 0.15
            val env 		      = EnvGen.ar( Env.linen( fadeIn, dur, fadeOut, 1, sinShape ), gate, doneAction = 0 )
            val recLevel0     = env.sqrt
            val preLevel0     = (1 - env).sqrt
            val recLevel      = recLevel0 * rec
            val preLevel      = preLevel0 * (1 - pre) + pre
            val run           = recLevel > 0
            RecordBuf.ar( in, bufID, off, recLevel, preLevel, run, 1 )
            LocalOut.kr( Impulse.kr( 1.0 / (dur + fadeIn + fadeOut ).max( 0.01 )))

      	   val speed      = pspeed.ar
			   val play		   = PlayBuf.ar( numChannels, bufID, speed, loop = 1 )
            mix( in, play, pmix )
      	}
      }

      filter( "*" ) {
         val pmix = pMix
         val bin2  = pAudioIn( "in2" )
         graph { in =>
            val in2  = bin2.ar
            val flt  = in * in2
            mix( in, flt, pmix )
         }
      }

      filter( "gain" ) {
         val pgain   = pAudio( "gain", ParamSpec( -30, 30 ), 0 )
         val pmix = pMix
         graph { in =>
            val amp  = pgain.ar.dbamp
            val flt  = in * amp
            mix( in, flt, pmix )
         }
      }

      filter( "gendy" ) {
         val pamt = pAudio( "amt", ParamSpec( 0, 1 ), 1 )
         val pmix = pMix
         graph { in =>
            val amt     = Lag.ar( pamt.ar, 0.1 )
            val minFreq	= amt * 69 + 12;
            val scale	= amt * 13 + 0.146;
            val gendy   = Gendy1.ar( 2, 3, 1, 1,
                     minFreq = minFreq, maxFreq = minFreq * 8,
                     ampScale = scale, durScale = scale,
                     initCPs = 7, kNum = 7 ) * in
            val flt	   = Compander.ar( gendy, gendy, 0.7, 1, 0.1, 0.001, 0.02 )
            mix( in, flt, pmix )
         }
      }

      filter( "~skew" ) {
         val plo  = pAudio( "lo", ParamSpec( 0, 1 ), 0 )
         val phi  = pAudio( "hi", ParamSpec( 0, 1 ), 1 )
         val ppow = pAudio( "pow", ParamSpec( 0.125, 8, ExpWarp ), 1 )
         val prnd = pAudio( "rnd", ParamSpec( 0, 1 ), 0 )

         val pmix = pMix
         graph { in =>
            val sig = in.clip2( 1 ).linlin( -1, 1, plo.ar, phi.ar ).pow( ppow.ar ).round( prnd.ar ) * 2 - 1
            mix( in, sig, pmix )
         }
      }

//      filter( "~onsets" ) {
//         val pthresh = pControl( "thresh", ParamSpec( 0, 1 ), 0.5 )
//         val pdecay  = pAudio( "decay",  ParamSpec( 0, 1 ), 0 )
//
//         val pmix = pMix
//         graph { in =>
//            val numChannels   = in.numOutputs
//            val bufIDs        = List.fill( numChannels )( bufEmpty( 1024 ).id )
//            val chain1 		   = FFT( bufIDs, in )
//            val onsets        = Onsets.kr( chain1, pthresh.kr )
//            val sig           = Decay.ar( Trig1.ar( onsets, SampleDur.ir ), pdecay.ar ).min( 1 ) // * 2 - 1
//            mix( in, sig, pmix )
//         }
//      }

      filter( "m-above" ) {
         val pthresh = pAudio( "thresh", ParamSpec( 1.0e-3, 1.0e-1, ExpWarp ), 1.0e-2 )
         val pmix = pMix
         graph { in =>
            val numChannels   = in.numOutputs
            val thresh		   = A2K.kr( pthresh.ar )
            val env			   = Env( 0.0, List( EnvSeg( 0.2, 0.0, stepShape ), EnvSeg( 0.2, 1.0, linShape )))
            val ramp			   = EnvGen.kr( env )
            val volume		   = LinLin.kr( thresh, 1.0e-3, 1.0e-1, 32, 4 )
            val bufIDs        = List.fill( numChannels )( bufEmpty( 1024 ).id )
            val chain1 		   = FFT( bufIDs, HPZ1.ar( in ))
            val chain2        = PV_MagAbove( chain1, thresh )
            val flt			   = LPZ1.ar( volume * IFFT( chain2 )) * ramp

            // account for initial dly
            val env2          = Env( 0.0, List( EnvSeg( BufDur.kr( bufIDs ) * 2, 0.0, stepShape ), EnvSeg( 0.2, 1, linShape )))
            val wet			   = EnvGen.kr( env2 )
            val sig			   = (in * (1 - wet).sqrt) + (flt * wet)
            mix( in, sig, pmix )
         }
      }

      filter( "m-below" ) {
         val pthresh = pAudio( "thresh", ParamSpec( 1.0e-2, 1.0e-0, ExpWarp ), 1.0e-1 )
         val pmix = pMix
         graph { in =>
            val numChannels   = in.numOutputs
            val thresh		   = A2K.kr( pthresh.ar )
            val env			   = Env( 0.0, List( EnvSeg( 0.2, 0.0, stepShape ), EnvSeg( 0.2, 1.0, linShape )))
            val ramp			   = EnvGen.kr( env )
//            val volume		   = LinLin.kr( thresh, 1.0e-3, 1.0e-1, 32, 4 )
            val volume		   = LinLin.kr( thresh, 1.0e-2, 1.0e-0, 4, 1 )
            val bufIDs        = List.fill( numChannels )( bufEmpty( 1024 ).id )
//            val chain1 		   = FFT( bufIDs, HPZ1.ar( in ))
            val chain1 		   = FFT( bufIDs, in )
            val chain2        = PV_MagBelow( chain1, thresh )
//            val flt			   = LPZ1.ar( volume * IFFT( chain2 )) * ramp
            val flt			   = volume * IFFT( chain2 )

            // account for initial dly
            val env2          = Env( 0.0, List( EnvSeg( BufDur.kr( bufIDs ) * 2, 0.0, stepShape ), EnvSeg( 0.2, 1, linShape )))
            val wet			   = EnvGen.kr( env2 )
            val sig			   = (in * (1 - wet).sqrt) + (flt * wet)
            mix( in, sig, pmix )
         }
      }

      filter( "pitch" ) {
         val ptrans  = pAudio( "shift", ParamSpec( 0.125, 4, ExpWarp ), 1 )
         val ptime   = pAudio( "time",  ParamSpec( 0.01, 1, ExpWarp ), 0.1 )
         val ppitch  = pAudio( "pitch", ParamSpec( 0.01, 1, ExpWarp ), 0.1 )
         val pmix    = pMix
         graph { in =>
            val grainSize  = 0.5f
            val pitch	   = A2K.kr( ptrans.ar )
            val timeDisp	= A2K.kr( ptime.ar )
            val pitchDisp	= A2K.kr( ppitch.ar )
            val flt		   = PitchShift.ar( in, grainSize, pitch, pitchDisp, timeDisp * grainSize )
            mix( in, flt, pmix )
         }
      }

      filter( "pow" ) {
         val pamt = pAudio( "amt", ParamSpec( 0, 1 ), 0.5 )
         val pmix = pMix
         graph { in =>
            val amt  = pamt.ar
            val amtM = 1 - amt
            val exp  = amtM * 0.5 + 0.5
            val flt0 = in.abs.pow( exp ) * in.signum
            val amp0 = Amplitude.ar( flt0 )
            val amp  = amtM + (amp0 * amt)
//            val flt  = LeakDC.ar( flt0 ) * amp
            val flt  = flt0 * amp
            mix( in, flt, pmix )
         }
      }

      filter( "verb" ) {
         val pextent = pScalar( "size", ParamSpec( 0, 1 ), 0.5 )
         val pcolor  = pControl( "color", ParamSpec( 0, 1 ), 0.5 )
         val pmix    = pMix
         graph { in =>
            val extent     = pextent.ir
            val color	   = Lag.kr( pcolor.kr, 0.1 )
            val i_roomSize	= LinExp.ir( extent, 0, 1, 1, 100 )
            val i_revTime  = LinExp.ir( extent, 0, 1, 0.3, 20 )
            val spread	   = 15
            val numChannels= in.numOutputs
            val ins        = in.outputs
            val verbs      = (ins :+ ins.last).grouped( 2 ).toSeq.flatMap( pair =>
               (GVerb.ar( Mix( pair ), i_roomSize, i_revTime, color, color, spread, 0, 1, 0.7, i_roomSize ) * 0.3).outputs
            )
// !! BUG IN SCALA 2.8.0 : CLASSCASTEXCEPTION
// weird stuff goin on with UGenIn seqs...
            val flt: GE     = Vector( verbs.take( numChannels ): _* ) // drops last one if necessary
            mix( in, flt, pmix )
         }
      }

      filter( "zero" ) {
         val pwidth	= pAudio( "width", ParamSpec( 0, 1 ), 0.5 )
         val pdiv 	= pAudio( "div",   ParamSpec( 1, 10, LinWarp, 1 ), 1 )
         val plag	   = pAudio( "lag",   ParamSpec( 0.001, 0.1, ExpWarp ), 0.01 )
         val pmix    = pMix
         graph { in =>
            val freq		= ZeroCrossing.ar( in ).max( 20 )
            val width0  = Lag.ar( pwidth.ar, 0.1 )
            val amp		= width0.sqrt
            val width	= width0.reciprocal
            val div		= Lag.ar( pdiv.ar, 0.1 )
            val lagTime	= plag.ar
            val pulse   = Lag.ar( LFPulse.ar( freq / div, 0, width ) * amp, lagTime )
            val flt		= in * pulse
            mix( in, flt, pmix )
         }
      }

      // -------------- DIFFUSIONS --------------

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
            val res = MIC_AND_PEOPLE.map { group =>
               val (_, off, numIn)  = group
println( "OFF = " + off + ", numIn = " + numIn )
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
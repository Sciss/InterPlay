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
import de.sciss.osc.OSCBundle
import java.util.Locale

object SoundProcesses {
   val ONSET_THRESH  = 0.5 // 0.3
   val diskBufSize   = 32768
   val liveDur       = 3.0 // 3.5 // 3.0    // minutes
   val totalDur      = 7.0 // 8.0 // 7.0 // 6.0    // minutes

//   val LIVE_AMP_SPEC = ParamSpec( 0.1, 10, ExpWarp ) -> 0.5
   val LIVE_AMP_SPEC = ParamSpec( 0.0, 0.6, LinWarp ) -> 0.25 // 3 // 3333

   import AnalysisBuffer.{anaChans, anaWinStep}
   val maxLiveAnaFr   = {
      val secs = liveDur * 60
      val fr0  = (secs * SAMPLE_RATE + 0.5).toLong
      val f1   = fr0 - (fr0 % diskBufSize )
      assert( f1 % anaWinStep == 0 )
      (f1 / anaWinStep).toInt
   }
   val maxLiveFrames    = maxLiveAnaFr * anaWinStep
   val maxLiveDur       = maxLiveFrames / SAMPLE_RATE // 10.0 * 60
   var liveBuf: Buffer  = _
//   var liveBufPhaseBus: AudioBus = _
//   val micChanIndex     = 0
//   val micNumChans      = 1

   var synPostM: Synth = _
   private var hpSynth : Synth = _

//   val anaClientBuf    = ByteBuffer.allocateDirect( maxLiveAnaFr * anaChans * 4 ).asFloatBuffer
   val anaClientBuf    = new AnalysisBuffer( maxLiveAnaFr, anaChans, SAMPLE_RATE / anaWinStep ) // .asFloatBuffer
   val anaMarkers      = new AnalysisMarkers // ISortedSet.empty[ Int ]

   val livePath         = new File( REC_PATH, "live" )
   var playPath         = LIVE_FILE.map( new File( livePath, _ )).orElse( livePath.listFiles( new FileFilter {
      def accept( p: File ) = {
         try {
            val spec = AudioFile.readSpec( p )
            (spec.numFrames > 0L) && (spec.numChannels == MIC_NUMCHANNELS)
         } catch {
            case _ => false
         }
      }
   }).toList.sortBy( _.getName ).lastOption )

   var pLive: Proc = _
   var pLiveHlb: Proc = _
   var pLiveDiff: Proc = _
   var collLive: Proc = _
   var collInt: Proc = _
   var collAll: Proc = _

   def init( s: Server )( implicit tx: ProcTxn ) {
      liveBuf           = Buffer.alloc( s, maxLiveFrames, MIC_NUMCHANNELS )
//      liveBufPhaseBus   = Bus.audio( s, 1 )

      // -------------- GENS --------------

      val genLive = gen( "live" ) {
         val pmode      = pScalar( "mode", ParamSpec( 0, 1, LinWarp, 1 ), LIVE_MODE )
         val ppos       = pScalar( "pos", ParamSpec( 0, 1 ), 0 )
         val pthresh    = pControl( "thresh", ParamSpec( 0, 1 ), ONSET_THRESH )
         val cntUpd     = math.max( 1, (maxLiveAnaFr / maxLiveDur).toInt )
         val df = new SimpleDateFormat( "'live'yyMMdd'_'HHmmss'.irc'", Locale.US )

         import AnalysisBuffer._

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
//            val phase   = DelTapWr.ar( liveBuf.id, in )
//            Out.ar( liveBufPhaseBus.index, phase )
            RecordBuf.ar( in, liveBuf.id, loop = 0 /*, doneAction = pauseSelf */ )
            val chain   = FFT( bufEmpty( anaFFTSize ).id, in, anaFFTOver.reciprocal )
            val coeffs  = MFCC.kr( chain, numMelCoeffs )
            val onsets  = Onsets.kr( chain, pthresh.kr )
            val fftTrig = Impulse.kr( SampleRate.ir / anaWinStep )
            val fftCnt  = PulseCount.kr( fftTrig )

//            SendReply.kr( fftTrig, coeffs, "/ana" )
            val me = Proc.local
            val frame = anaClientBuf.emptyFrame
            fftTrig.react( fftCnt +: onsets +: coeffs.outputs ) { data =>
               val iter    = data.iterator
               val cnt     = iter.next.toInt - 1
               val onset   = iter.next > 0
//println( "cnt = " + cnt )
               if( cnt < maxLiveAnaFr ) {
                  var i = 0; while( i < numMelCoeffs ) {
                     frame( i ) = (iter.next.toFloat + normAdd( i )) * normMul( i )
                  i += 1 }
                  anaClientBuf.setFrame( cnt, frame )
                  if( onset ) anaMarkers.add( cnt )
                  if( cnt % cntUpd == 0 ) Process.spawnAtomic( "live update pos" ) { implicit tx =>
                     me.control( "pos" ).v = cnt.toDouble / maxLiveAnaFr
                  }
               } else if( cnt == maxLiveAnaFr ) {
//println( "STOPPENDORFER 1" )
                  Process.spawnAtomic( "live removal" ) { implicit tx =>
//println( "STOPPENDORFER 2" )
                     Process.removeAndDispose( "live removal", pLiveDiff, 5.0, postFun = _ => println( "LIVE DONE" ))
//println( "STOPPENDORFER 3" )
//                     me.stop
//                     me.control( "pos" ).v = 1.0
                  }
               }
            }

//            PauseSelf.kr( fftCnt === maxLiveAnaFr )
//            PauseSelf.kr( fftCnt > maxLiveAnaFr )

            in
         }
      }

      gen( "test" ) {
         graph {
            PinkNoise.ar( LFPulse.kr( 1 ))
         }
      }

      // -------------- FILTERS --------------

      def mix( in: GE, flt: GE, mix: ProcParamAudio ) : GE = LinXFade2.ar( in, flt, mix.ar * 2 - 1 )
      def pMix = pAudio( "mix", ParamSpec( 0, 1 ), 1 )

      // -------------- DIFFUSIONS --------------

      val diffMute = diff( "O-mute" ) {
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
               IIdxSeq.fill( chanOff )( Constant( 0 )) ++ sig.outputs ++ IIdxSeq.fill( masterBus.numChannels - (numCh + chanOff) )( Constant( 0 ))
            }
         }

         diff( "D-all" + suff ) {
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

         diff( "D-one" + suff ) {
            val pamp  = pAudio( "amp", ParamSpec( 0.01, 10, ExpWarp ), 1 )
            val pidx  = pAudio( "idx", ParamSpec( 0, numCh - 1, LinWarp, 1 ), 0 )
            val pout  = pAudioOut( "out", None )

            graph { in =>
               val sig           = (in * Lag.ar( pamp.ar, 0.1 )).outputs
               val inChannels    = sig.size
               val outChannels   = numCh
               val idx           = Lag.ar( pidx.ar, 0.1 )
               val outSig        = IIdxSeq.tabulate( outChannels )( ch =>
                  sig( ch % inChannels ) * (1 - idx.absdif( ch ).min( 1 )))
               pout.ar( placeChannels( outSig ))
            }
         }
      }

      filter( "O-all" ) {
//         val pamp  = pAudio( "amp", ParamSpec( 0.01, 10, ExpWarp ), 1 )
//         val pout  = pAudioOut( "out", None )
         val pamp  = pAudio( "amp", ParamSpec( 0, 10 ), 1 )

         graph { in =>
            val sig          = (in * Lag.ar( pamp.ar, 0.1 )).outputs
            val inChannels   = sig.size
            val outChannels  = MASTER_NUMCHANNELS
            val outSig       = IIdxSeq.tabulate( outChannels )( ch => sig( ch % inChannels ))
//            pout.ar( placeChannels( outSig ))
            outSig
         }
      }

      filter( "O-one" ) {
//         val pamp  = pAudio( "amp", ParamSpec( 0.01, 10, ExpWarp ), 1 )
         val pamp  = pAudio( "amp", ParamSpec( 0, 10 ), 1 )
         val pidx  = pAudio( "idx", ParamSpec( 0, MASTER_NUMCHANNELS - 1, LinWarp, 1 ), 0 )

         graph { in =>
            val sig           = (in * Lag.ar( pamp.ar, 0.1 )).outputs
            val inChannels    = sig.size
            val outChannels   = MASTER_NUMCHANNELS
            val idx           = Lag.ar( pidx.ar, 0.1 )
            val outSig        = IIdxSeq.tabulate( outChannels )( ch =>
               sig( ch % inChannels ) * (1 - idx.absdif( ch ).min( 1 )))
            //placeChannels( outSig )
            outSig
         }
      }

      filter( "O-pan" ) {
         val pamp    = pAudio( "amp", ParamSpec( 0, 10 ), 1 )
         val pspread = pControl( "spr",  ParamSpec( 0.0, 1.0 ), 0.25 ) // XXX rand
         val prota   = pControl( "rota", ParamSpec( 0.0, 1.0 ), 0.0 )
         val pbase   = pControl( "azi",  ParamSpec( 0.0, 360.0 ), 0.0 )
         val pspeed  = pControl( "speed",  ParamSpec( 0.01, 10.0, ExpWarp ), 0.1 )
//         val pout    = pAudioOut( "out", None )

         graph { in =>
            val baseAzi       = Lag.kr( pbase.kr, 0.5 ) + IRand( 0, 360 )
            val rotaAmt       = Lag.kr( prota.kr, 0.1 )
            val spread        = Lag.kr( pspread.kr, 0.5 )
            val inChannels   = in.numOutputs
            val outChannels  = MASTER_NUMCHANNELS // numCh
//            val sig1         = List.tabulate( outChannels )( ch => sig( ch % inChannels ))
            val rotaSpeed     = pspeed.kr // 0.1
            val inSig         = (in * Lag.ar( pamp.ar, 0.1 )).outputs
            val noise         = LFDNoise1.kr( rotaSpeed ) * rotaAmt * 2
            val outSig0: Array[ GE ] = Array.fill( outChannels )( 0 )
            val altern        = false
            for( inCh <- 0 until inChannels ) {
               val pos0 = if( altern ) {
                  (baseAzi / 180) + (inCh / outChannels * 2);
               } else {
                  (baseAzi / 180) + (inCh / inChannels * 2);
               }
               val pos = pos0 + noise

               // + rota
//				   w	 = inCh / (inChannels -1);
//				   level = ((1 - levelMod) * w) + (1 - w);
               val level   = 1   // (1 - w);
               val width   = (spread * (outChannels - 2)) + 2
               val pan     = PanAz.ar( outChannels, inSig( inCh ), pos, level, width, 0 )
               pan.outputs.zipWithIndex.foreach( tup => {
                  val (chanSig, i) = tup
                  outSig0( i ) = outSig0( i ) + chanSig
               })
            }
            val outSig = outSig0.toSeq
//            if( METERS ) Out.ar( masterBusIndex, outSig )
//            pout.ar( placeChannels( outSig.toSeq ))
            outSig
         }
      }

      filter( "O-rnd" ) {
//         val pamp  = pAudio( "amp", ParamSpec( 0.01, 10, ExpWarp ), 1 )
         val pamp    = pAudio( "amp", ParamSpec( 0, 10 ), 1 )
         val pfreq = pControl( "freq", ParamSpec( 0.01, 10, ExpWarp ), 0.1 )
         val ppow  = pControl( "pow", ParamSpec( 1, 10 ), 2 )
         val plag  = pControl( "lag", ParamSpec( 0.1, 10 ), 1 )
//         val pout  = pAudioOut( "out", None )

         graph { in =>
            val sig          = (in * Lag.ar( pamp.ar, 0.1 )).outputs
            val inChannels   = sig.size
            val outChannels  = MASTER_NUMCHANNELS // numCh
            val sig1: GE     = List.tabulate( outChannels )( ch => sig( ch % inChannels ))
            val freq         = pfreq.kr
            val lag          = plag.kr
            val pw           = ppow.kr
            val rands        = Lag.ar( TRand.ar( 0, 1, Dust.ar( List.fill( outChannels )( freq ))).pow( pw ), lag )
            val outSig       = sig1 * rands
//            if( METERS ) Out.ar( masterBusIndex, outSig )
//            pout.ar( placeChannels( outSig ))
            outSig
         }
      }

      val filtThru = filter( "O-thru" ) {
         graph { in =>
            require( in.numOutputs == MASTER_NUMCHANNELS )
            in
         }
      }
      val diffThru = diff( "D-thru" ) {
//         val pin   = pAudioIn( "in", Some( RichBus.audio( s, MASTER_NUMCHANNELS )))
         val pout  = pAudioOut( "out", None )
         graph { in =>
            require( in.numOutputs == MASTER_NUMCHANNELS )
            pout.ar( in )
         }
      }
      val factCollAll = diff( "D-mast" ) {
         val pgate   = pControl( "gate", ParamSpec( 0, 1, LinWarp, 1 ), 1 )
         val pout    = pAudioOut( "out", None )
         graph { in =>
            require( in.numOutputs == MASTER_NUMCHANNELS )
            val liveSecs   = liveDur * 60
            val remainSecs = totalDur * 60 - liveSecs
            val climax     = remainSecs - 60
            val release    = remainSecs - climax
            val boost      = (2.5).dbamp
            val env        = EnvGen.kr( new Env( 1,
               List( EnvSeg( liveSecs, 1 ), EnvSeg( climax, boost ), EnvSeg( release, 1 )), releaseNode = 0 ), gate = pgate.kr )
            val flt        = in * env
            pout.ar( flt )
         }
      }

      collInt = filtThru.make
      collAll = factCollAll.make // diffThru.make
      val dummy = (gen( "dummy" ) { graph {
//        Silent.ar( MASTER_NUMCHANNELS )
        DC.ar( Seq.fill( MASTER_NUMCHANNELS )( 0 ))
      }}).make
      dummy ~> collInt ~> collAll
      ProcessHelper.playNewDiff( collAll, postFun = dummy.dispose( _ )) // dummy needed to get the input channel :-(

      diff( "mitschnitt" ) {
         val df = new SimpleDateFormat( "'rec'yyMMdd'_'HHmmss'.irc'", Locale.US )
         graph { in =>
            val recPath = new File( new File( REC_PATH, "mitschnitt" ), df.format( new java.util.Date() ))
            DiskOut.ar( bufRecord( recPath.getAbsolutePath, in.numOutputs, AudioFileType.IRCAM ).id, in )
         }
      }

      def recMix( sig: GE, numOut: Int ) : GE = {
         val numIn = masterBus.numChannels
         val sig1: GE = if( numOut == numIn ) {
            sig
         } else if( numIn == 1 ) {
            Vector.fill[ GE ]( numOut )( sig )
         } else {
            val sigOut  = Array.fill[ GE ]( numOut )( 0.0f )
            val sca     = (numOut - 1).toFloat / (numIn - 1)
            sig.outputs.zipWithIndex.foreach { tup =>
               val (sigIn, inCh) = tup
               val outCh         = inCh * sca
               val fr            = outCh % 1f
               val outChI        = outCh.toInt
               if( fr == 0f ) {
                  sigOut( outChI ) += sigIn
               } else {
                  sigOut( outChI )     += sigIn * (1 - fr).sqrt
                  sigOut( outChI + 1 ) += sigIn * fr.sqrt
               }
            }
            Limiter.ar( sigOut.toSeq, (-0.2).dbamp )
         }
         assert( sig1.numOutputs == numOut )
         sig1
      }

      val dfPostM = SynthDef( "post-master" ) {
         val sig = In.ar( masterBus.index, masterBus.numChannels )
         // externe recorder
         REC_CHANGROUPS.foreach { group =>
            val (name, off, numOut) = group
            Out.ar( off, recMix( sig, numOut ))
         }
         // master + people meters
         val meterTr    = Impulse.kr( 20 )
         //val trigA    = Trig1.ar( meterTr, SampleDur.ir )
         val (peoplePeak, peopleRMS) = {
            val res = MIC_AND_PEOPLE.map { group =>
               val (_, off, numIn)  = group
//println( "OFF = " + off + ", numIn = " + numIn )
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

      // init live
      pLive    = genLive.make
//      pLiveOut = diffMute.make
//      pLiveHlb = fltHilb.make
//      pLiveHlb.control( "freq" ).v = -1e-6
      pLiveHlb = (filter( "fb" ) {
         val pup        = pControl( "up", ParamSpec( 0.1, 10, ExpWarp ), 2 )
         val pdn        = pControl( "dn", ParamSpec( 0.1, 10, ExpWarp ), 4 )
         val pathresh   = pControl( "athr", ParamSpec( 0.001, 1, ExpWarp ), 0.003 )
         val ppthresh   = pControl( "pthr", ParamSpec( 0.1, 1, ExpWarp ), 0.5 )
         graph { in =>
            val flt0 = MidEQ.ar( in, 1328, 1.0/10, -18 )
            def stage( in: GE ) = {
               val Seq( freq, hasPitch ) = Pitch.kr( in,
                  initFreq = 441,
                  minFreq  = 441,
                  maxFreq  = 16000,
                  execFreq = 441,
                  ampThresh = 0.05,
                  peakThresh = 0.2 ).outputs
               val q = freq.linlin( 441, 16000, 9, 2 ) + 1
               MidEQ.ar( in, freq, q.reciprocal, LagUD.kr( hasPitch * -18, pup.kr, pdn.kr ))
            }
            stage( stage( stage( flt0 )))
         }
      }).make

      pLiveDiff = (filter( "O-live" ) {
         val pamp  = pAudio( "amp", LIVE_AMP_SPEC._1, LIVE_AMP_SPEC._2 )

         val pfreq = pControl( "freq", ParamSpec( 0.01, 10, ExpWarp ), 0.1 )
         val ppow  = pControl( "pow", ParamSpec( 1, 10 ), 2 )
         val plag  = pControl( "lag", ParamSpec( 0.1, 10 ), 1 )

         graph { in =>
            val sig          = (in * Lag.ar( pamp.ar, 0.1 )).outputs
            val inChannels   = sig.size
            val outChannels  = MASTER_NUMCHANNELS

            val sig1: GE     = List.tabulate( outChannels )( ch => sig( ch % inChannels ))
            val freq         = pfreq.kr
            val lag          = plag.kr
            val pw           = ppow.kr
            val rands        = Lag.ar( TRand.ar( 0, 1, Dust.ar( List.fill( outChannels )( freq ))).pow( pw ), lag )
            val outSig       = sig1 * rands
//            if( METERS ) Out.ar( masterBusIndex, outSig )
//            pout.ar( placeChannels( outSig ))
//            val outSig       = IIdxSeq.tabulate( outChannels )( ch => sig( ch % inChannels ))
            outSig
         }
      }).make
      pLiveDiff.control( "freq" ).v = 0.05
      pLiveDiff.control( "pow" ).v  = 0.5
      pLiveDiff.control( "lag" ).v  = 10

//      pLiveDiff = diffAll.make // factory( "D-all" ).make
      collLive = diffThru.make
      pLive ~> pLiveHlb ~> pLiveDiff ~> collLive ~> collAll

      val dfHP = SynthDef( "hp-mix" ) {
         val sig = In.ar( masterBus.index, masterBus.numChannels )
         ReplaceOut.ar( SOLO_OFFSET, recMix( sig, SOLO_NUMCHANNELS ))
      }
      hpSynth = Synth( s )
      dfHP.recv( s, OSCBundle(
         hpSynth.newMsg( dfHP.name, synPostM, addAction = addAfter ),
         hpSynth.runMsg( false )
      ))

//      Process.init
   }

   private val liveStarted = Ref( false )
   @volatile private var logicalTime0 = 0L
   def startLive( implicit tx: ProcTxn ) {
      val wasStarted = liveStarted.swap( true )
      if( wasStarted ) return

      if( AUTO_RECORD ) mitschnitt( true )

      if( !collLive.isPlaying ) xfade( 3.0 ) { collLive.play }
      if( !pLiveDiff.isPlaying ) pLiveDiff.play
      if( !pLiveHlb.isPlaying )  pLiveHlb.play
      if( !pLive.isPlaying )     pLive.play
      collAll.control( "gate" ).v = 0  // "release" the envelope
      tx.afterCommit { _ =>
         logicalTime0 = System.currentTimeMillis
      }
      Process.init
   }

   def stopProcesses( implicit tx: ProcTxn ) {
      if( liveStarted() ) Process.stopAll
   }

   def logicalTime() : Double = {
//      val liveDur = anaClientBuf.framesWritten.toDouble * anaWinStep / SAMPLE_RATE
      if( logicalTime0 == 0L ) 0.0 else {
         val mins = (System.currentTimeMillis() - logicalTime0).toDouble / 60000
         if( mins <= liveDur ) mins / liveDur else math.min( 2.0, (mins - liveDur) / (totalDur - liveDur) + 1.0 )
      }
   }

   def headphoneMix( onOff: Boolean ) {
      hpSynth.run( onOff )
//      require( EventQueue.isDispatcherThread )
//      hpSynth = (onOff, hpSynth) match {
//         case (false, Some( synth )) => synth.free; None
//         case (true, None) => Some( Synth.after( s.defaultGroup, "hp-mix" ))
//         case (_, x) => x
//      }
   }

   private val mitRef = Ref( Option.empty[ Proc ])
   def mitschnitt( onOff: Boolean )( implicit tx: ProcTxn ) {
      val p = mitRef.swap( None )
      p.foreach( Process.removeAndDisposeDiff( _ ))
      if( onOff ) {
         val p = factory( "mitschnitt" ).make
         collAll ~> p
         p.play
         mitRef.set( Some( p ))
      }
   }

//   def factory( name: String )( implicit tx: ProcTxn ) =
//      ProcDemiurg.factories.find( _.name == name ).getOrElse( error( "Factory not found: '" + name + "'" ))
}
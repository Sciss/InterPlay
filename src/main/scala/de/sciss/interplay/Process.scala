/*
 *  Process.scala
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

import InterPlay._
import SoundProcesses._
import de.sciss.synth.Model
import collection.immutable.{IndexedSeq => IIdxSeq, Set => ISet, SortedSet => ISortedSet}
import actors.Actor
import java.util.{TimerTask, Timer}
import de.sciss.synth.io.AudioFile
import java.io.IOException
import edu.stanford.ppl.ccstm.Txn
import de.sciss.synth.proc.{ProcDemiurg, DSL, Proc, TxnModel, Ref, ProcTxn}
import DSL._

object Process {
   val verbose = true
   val all = List( ProcSehen, ProcHoeren, ProcRiechen, ProcSchmecken, ProcTasten, ProcOrientieren, ProcGleichgewichten, ProcKoerper )
//   val all = List( ProcRiechen )
//   val all = List( ProcHoeren )

   private val actor = new Actor { def act = loop { react {
      case d: Do => try {
         d.perform
      } catch {
         case e =>
            println( "Caught exception in actor:" )
            e.printStackTrace()
      }
   }}}

   sealed trait ReplacePoint
   case object ReplaceInternal extends ReplacePoint
   case object ReplaceLive     extends ReplacePoint
   case object ReplaceAll      extends ReplacePoint

   def init( implicit tx: ProcTxn ) {
      actor.start  // save to call repeatedly, so no prob with tx rollback
      all.foreach( _.init )
   }

   def stopAll( implicit tx: ProcTxn ) {
      all.foreach( _.stop )
   }

   def secsToFrames( secs: Double ) = (secs * (SAMPLE_RATE / AnalysisBuffer.anaWinStep)).toInt
   def framesToPos( idx: Int )      = idx.toDouble / (anaClientBuf.numFrames - 1)
   def frameToSecs( idx: Int )      = idx.toDouble * AnalysisBuffer.anaWinStep / SAMPLE_RATE
   def secsToPos( secs: Double )    = framesToPos( secsToFrames( secs ))

   def afterCommit( tx: ProcTxn )( thunk: => Unit ) {
      require( tx.isActive, "Juhuuu. tx not active anymore" )
      tx.afterCommit( _ => thunk )
   }

   private lazy val delayTimer = new Timer( true )

   def delay( dur: Double )( thunk: => Unit )( implicit tx: ProcTxn ) /* : TimerTask = */ {
      afterCommit( tx ) {
         val res = new TimerTask {
            def run = thunk
         }
         val timer = delayTimer // val timer = new Timer( true )
         timer.schedule( res, (dur * 1000).toLong )
      }
//      res
   }

   def addTail( p: Proc, fadeTime: Double = 0.0 )( implicit tx: ProcTxn ) {
//      ProcessHelper.playNewDiff( fadeTime, p )
      p ~> collInt
//      if( fadeTime > 0 ) xfade( fadeTime ) { p.play } else p.play
//      ProcessHelper.playNewDiff( p, fadeTime )

      val ctrl = p.control( "amp" )
      val amp = ctrl.v

      lazy val pl: Proc.Listener = new Proc.Listener {
         def updated( u: Proc.Update ) {
            if( u.audioBusesConnected.find( b => (b.sourceVertex == p) && (b.out.name == "out") ).isDefined ) {
                // why we need spawn?? wolkenpumpe doesn't get the new state update otherwise. why?? XXX
               spawnAtomic( "addTail listener" ) { implicit tx =>
                  p.removeListener( pl )
//                  if( fadeTime > 0 ) xfade( fadeTime ) { p.play } else p.play
                  if( fadeTime > 0 ) {
                     ctrl.v = ctrl.spec.lo
                     if( !p.isPlaying ) p.play
                     glide( fadeTime ) { ctrl.v = amp }
                  } else if( !p.isPlaying ) p.play
//                  postFun( tx )
               }
            }
         }
      }
      p.addListener( pl )
   }

   private def replacePoint( p: ReplacePoint )( implicit tx: ProcTxn ) = p match {
      case ReplaceInternal => collInt
      case ReplaceLive     => require( liveActive ); collLive
      case ReplaceAll      => collAll
   }

   def liveActive( implicit tx: ProcTxn ) : Boolean = collLive.state.valid

   def canReplaceTail( point: ReplacePoint = ReplaceInternal )( implicit tx: ProcTxn ) : Boolean = {
      replacePoint( point ).audioInput( "in" ).edges.nonEmpty
   }

   def replaceTail( p: Proc, fadeTime: Double = 0.0, point: ReplacePoint = ReplaceInternal )( implicit tx: ProcTxn ) {
      replaceTailChain( p, p, p :: Nil, if( fadeTime > 0 ) Some( fadeTime :: Nil ) else None, point )
   }

//   def replaceTail( p: Proc, fadeTime: Double = 0.0, point: ReplacePoint = ReplaceInternal )( implicit tx: ProcTxn ) {
//      val coll    = replacePoint( point )
//      val oldIn   = coll.audioInput( "in" )
//      val es      = oldIn.edges
////      if( es.isEmpty ) return false
//      require( es.nonEmpty )
//      val newIn   = p.audioInput( "in" )
//      es.foreach { e =>
//         e.out ~/> oldIn
//         e.out ~> newIn
//      }
//      if( fadeTime > 0 ) {
//         p.bypass
//         p ~> coll
//         p.play
//         xfade( fadeTime ) { p.engage }
//      } else {
//         p ~> coll
//         p.play
//      }
//      true
//   }

   def replaceTailChain( pin: Proc, pout: Proc, pflt: Seq[ Proc ], fadeTimes: Option[ Seq[ Double ]] = None, point: ReplacePoint = ReplaceInternal )( implicit tx: ProcTxn ) {
      val coll    = replacePoint( point )
      val oldIn   = coll.audioInput( "in" )
      val es      = oldIn.edges

//val debug = false // pin != pout
//if( debug ) inform( "replace: pt = " + point + " ; oldIn.bus " + oldIn.bus )

//      if( es.isEmpty ) return false
      require( es.nonEmpty )
      val newIn   = pin.audioInput( "in" )
      es.foreach { e =>
//if( debug ) inform( "replace: connecting output " + e.out.name + " with bus " + e.out.bus )
         e.out ~/> oldIn
         e.out ~> newIn
      }
      fadeTimes match {
         case Some( fdts ) =>
            pflt.foreach( _.bypass )
            pout ~> coll
            pout.play
            pflt.zip( fdts ).foreach { tup =>
               val (p, fdt) =  tup
               xfade( fdt ) { p.engage }
            }
         case None =>
            pout ~> coll
            pout.play
      }
      true
   }

//   def replaceTailOrDispose( p: Proc, fadeTime: Double = 0.0 )( implicit tx: ProcTxn ) : Boolean = {
//      val res = replaceTail( p, fadeTime )
//      if( !res ) removeAndDispose( p )
//      res
//   }


   def timeString() = (new java.util.Date()).toString

   /**
    * Removes and disposes a subtree. That is, it disconnects the proc's
    * outputs, then the inputs, then disposes the proc, and recursively
    * does this for all the proc's inputs
    */
   def removeAndDispose( info: => String, p: Proc, fadeTime: Double = 0.0, postFun: ProcTxn => Unit = _ => () )( implicit tx: ProcTxn ) {
      if( fadeTime > 0 ) glide( fadeTime ) {
         val ctrl = p.control( "amp" )
         ctrl.v = ctrl.spec.lo
      }
      ProcessHelper.whenGlideDone( info, p, "amp" ) { implicit tx =>
         disposeSubTree( p )
         postFun( tx )
      }
   }

   /*
    * Removes and disposes subtree (without fading)
    */
   private def disposeSubTree( p: Proc )( implicit tx: ProcTxn ) {
      p.audioOutputs.flatMap( _.edges ).foreach( e => e.out ~/> e.in )
      val srcs: Set[ Proc ] = p.audioInputs.flatMap( _.edges ).map( e => {
         val pSrc = e.sourceVertex
         if( pSrc.isPlaying ) pSrc.stop
         e.out ~/> e.in
         pSrc
      })( collection.breakOut )
      p.dispose
      srcs.foreach( disposeSubTree( _ ))
   }

   private def collectSources( p: Proc, stop: Option[ Proc ] = None, set: ISet[ Proc ] = ISet.empty )( implicit tx: ProcTxn ) : ISet[ Proc ] = {
      val set1 = set + p
      if( Some( p ) == stop ) {
         set1
      } else {
         p.audioInputs.flatMap( _.edges ).foldLeft( set1 )( (seti, e) => collectSources( e.sourceVertex, stop, seti ))
      }
   }

   /**
    * Note: this still leaves a suspicious rollback-error printing... however all procs seem to get properly disposed,
    * so let's ignore that for the moment...
    */
   def removeAndDisposeChain( info: => String, pin: Proc, pout: Proc, fadeTime: Double = 0.0, preFun: ProcTxn => Unit = _ => (), postFun: ProcTxn => Unit = _ => () )( implicit tx: ProcTxn ) {
      val ctrl = pout.control( "amp" )

      def dispo( implicit tx: ProcTxn ) {
         preFun( tx )
         // first, reconnect the outer part
         val ines    = pin.audioInput( "in" ).edges
         val outes   = pout.audioOutput( "out" ).edges
         val outesf  = outes.filterNot( _.targetVertex.name.startsWith( "$" ))  // XXX tricky shit to determine the meters
         ines.foreach { ine =>
//            ine.out ~/> ine.in
            outesf.foreach( oute => ine.out ~> oute.in )
         }
         // tricky part: stop all sources in their reverse topological order
         // (because of some sucky bus behaviour)
         val srcs = collectSources( pout, Some( pin ), ISortedSet.empty( ProcDemiurg.worlds( pout.server ).topology.reverse ))
//println( "sources : " + srcs.toList.toString )
//         srcs.foreach( p => if( p.isPlaying ) p.stop )
//         // isolate chain
//         ines.foreach { ine =>
//            assert( ine.targetVertex == pin )
//            ine.out ~/> ine.in
//         }
//         outesf.foreach { oute =>
//            assert( oute.sourceVertex == pout )
//            oute.out ~/> oute.in
//         }
//         // then dispose it as a subtree beginning at pout
//         disposeSubTree( pout )
srcs.foreach { p =>
   p.dispose
}
         postFun( tx )
      }

      if( (ctrl.v == ctrl.spec.lo) || (fadeTime == 0.0) ) {
         dispo
      } else {
         glide( fadeTime ) { ctrl.v = ctrl.spec.lo }
         ProcessHelper.whenGlideDone( info, pout, ctrl.name )( dispo( _ ))
      }
   }

   /**
    * Removes and disposes an auxiliary diff. Simply disconnects
    * from the source, but does not otherwise touch or free the source.
    * Does not pay attention to outputs!
    */
   def removeAndDisposeDiff( p: Proc )( implicit tx: ProcTxn ) {
      p.audioInputs.flatMap( _.edges ).foreach { e => e.out ~/> e.in }
      p.dispose
   }

   def waitForAnalysis( minDur: Double )( thunk: => Unit )( implicit tx: ProcTxn ) {
      val minFrames = secsToFrames( minDur )
      afterCommit( tx ) {
         if( anaClientBuf.framesWritten >= minFrames ) {
            thunk
         } else {
            lazy val l: Model.Listener = {
               case AnalysisBuffer.FrameUpdated( idx, last ) => if( idx >= minFrames ) {
                  atomic( "waitForAnalysis listener" ) { implicit tx =>
                     anaClientBuf.removeListener( l )
                     afterCommit( tx )( thunk )
                  }
               }
            }
            anaClientBuf.addListener( l )
         }
      }
   }

   private def blockWithTimeOut[ Z ]( info: => String, block: ProcTxn => Z, tx: ProcTxn ) : Z = {
      val timeOut = new java.util.Timer( true )
      timeOut.schedule( new TimerTask {
         def run {
            informDir( "Timeout for " + info, force = true )
         }
      }, 4000L )
      try {
         block( tx )
      } finally {
         timeOut.cancel()
      }
   }

   def atomic[ Z ]( info: => String )( block: ProcTxn => Z ) : Z = {
      ProcTxn.atomic { tx =>
         blockWithTimeOut( info, block, tx )
      }
   }

   def spawnAtomic[ Z ]( info: => String )( block: ProcTxn => Z ) {
      ProcTxn.spawnAtomic { tx =>
         blockWithTimeOut( info, block, tx )
      }
   }

   private def doInform( what: => String )( implicit tx: ProcTxn ) {
      if( tx.isActive ) tx.afterCommit( _ => printWithTime( what ))
      else printWithTime( what )
   }

   private def printWithTime( what: String ) {
      println( timeString() + " " + what )
   }

   private def inform( what: => String, force: Boolean = false )( implicit tx: ProcTxn ) = if( verbose || force ) {
      doInform( "Process : " + what )
   }

   private def informDir( what: => String, force: Boolean = false ) = if( verbose || force ) {
       printWithTime( what )
   }

   /**
    *  @return the number of corresponding _analysis_ frames (e.g. at sr/512) available right now
    *    from both the AnalysisBuffer and the live soundfile. Zero if the soundfile cannot be accessed
    */
   def availableLiveRecordingFrames : Int = {
      playPath.map { inPath =>
         try {
//            val spec = audioFileSpec( inPath.getAbsolutePath() ) // AudioFile.readSpec( inPath )
            // IMPORTANT: do _not_ use spec because that currently doesn't respect changing files!
            val spec = AudioFile.readSpec( inPath )
            math.min( anaClientBuf.framesWritten, spec.numFrames / AnalysisBuffer.anaWinStep ).toInt
         } catch {
            case e => 0
         }
      } getOrElse 0
   }

   private var truncCache = Map.empty[ Int, String ]

   /**
    * @param   done  is called with an Option to the audiofile name. None if an error occured
    */
   def truncateLiveRecording( numAnaFrames: Int )( done: Option[ String ] => Unit )( implicit tx: ProcTxn ) {
      spawn( actTruncLiveRecording( numAnaFrames )( done ))
   }

   private def actTruncLiveRecording( numAnaFrames: Int )( done: Option[ String ] => Unit ) {
      val pathO: Option[ String ] = truncCache.get( numAnaFrames ).orElse( playPath.flatMap( inPath => try {
         val afIn          = AudioFile.openRead( inPath )
         val numFrames     = numAnaFrames.toLong * AnalysisBuffer.anaWinStep
         if( numFrames > afIn.numFrames ) throw new IOException( "Not enough available frames" )
         val outF          = if( numFrames == afIn.numFrames ) {
            inPath
         } else {
            val afOut      = FScape.createTempAudioFile( afIn )
            val buf        = afIn.frameBuffer( 8192 )
            var remaining  = numFrames
            while( remaining > 0L ) {
               val chunkLen   = math.min( 8192, remaining ).toInt
               afIn.readFrames( buf, 0, chunkLen )
               afOut.writeFrames( buf, 0, chunkLen )
               remaining -= chunkLen
            }
            afOut.close
            afOut.file.get
         }
         afIn.close
         val outPath = outF.getAbsolutePath()
         truncCache += numAnaFrames -> outPath
         Some( outPath )

      } catch {
         case e =>
            informDir( "Failed to copy live rec file. Reason:", force = true )
            e.printStackTrace()
            None
      }).orElse( None ))
      done( pathO )
   }

   def searchAnalysis( timeInteg: Double, maxResults: Int = 20, frameMeasure: Array[ Float ] => Float,
                       integMeasure: Array[ Float ] => Float, rotateBuf: Boolean = false )
                     ( fun: ISortedSet[ Sample ] => Unit )( implicit tx: ProcTxn ) {
      require( maxResults > 0, "maxResults must be > 0, but is " + maxResults )
      spawn( actSearchAna( timeInteg, maxResults, frameMeasure, integMeasure, rotateBuf )( fun ))
   }

   private def actSearchAna( timeInteg: Double, maxResults: Int = 20, frameMeasure: Array[ Float ] => Float,
                             integMeasure: Array[ Float ] => Float, rotateBuf: Boolean = false )
                           ( fun: ISortedSet[ Sample ] => Unit ) {
      val frameInteg = secsToFrames( timeInteg )
      val buf        = anaClientBuf
      val chanBuf    = new Array[ Float ]( buf.numChannels )
      val timeBuf    = new Array[ Float ]( frameInteg )
      val numFrames  = buf.framesWritten - frameInteg + 1
      var res        = ISortedSet.empty[ Sample ]( sampleOrd )
      var resCnt     = 0
      informDir( "searchAnalysis started " + frameInteg + " / " + numFrames )

      def karlheinz( idx: Int ) {
         val m = integMeasure( timeBuf )
         if( resCnt < maxResults ) {
            res += Sample( idx, m )
            resCnt += 1
         } else if( res.last.measure > m ) {
            res = res.dropRight( 1 ) + Sample( idx, m )
         }
      }

      if( numFrames > 0 ) {
         var x = 0; while( x < frameInteg ) {
            buf.getFrame( 0, chanBuf )
            timeBuf( x ) = frameMeasure( chanBuf )
         x += 1 }
         karlheinz( 0 )
      }
      var off = 1; while( off < numFrames ) {
         val fm = frameMeasure( buf.getFrame( off, chanBuf ))
         if( rotateBuf ) {
            System.arraycopy( timeBuf, 1, timeBuf, 0, frameInteg - 1 )
            timeBuf( 0 ) = fm
         } else {
            timeBuf( (off - 1) % frameInteg ) = fm
         }
         karlheinz( off )
      off += 1 }

      informDir( "searchAnalysis done " + res.size )
      fun( res )
   }

   def searchAnalysisM( frameInteg: Int, maxResults: Int = 20, measure: Similarity.Mat => Float )
                      ( fun: ISortedSet[ Sample ] => Unit, rotateBuf: Boolean = false )( implicit tx: ProcTxn ) {
      require( maxResults > 0, "maxResults must be > 0, but is " + maxResults )
      spawn( actSearchAnaM( frameInteg, maxResults, measure )( fun ))
   }

   private def actSearchAnaM( frameInteg: Int, maxResults: Int = 20, measure: Similarity.Mat => Float )
                            ( fun: ISortedSet[ Sample ] => Unit, rotateBuf: Boolean = false ) {
      informDir( "searchAnalysisM started" )
      val buf        = anaClientBuf
      val numChannels= buf.numChannels
//         val frames     = Array.ofDim[ Float ]( frameInteg, numChannels )
      val frames     = Similarity.Mat( frameInteg, numChannels )
      val numFrames  = buf.framesWritten - frameInteg + 1
      var res        = ISortedSet.empty[ Sample ]( sampleOrd )
      var resCnt     = 0
      val frameIntegM= frameInteg - 1

      def karlheinz( idx: Int ) {
         val m = measure( frames )
         if( resCnt < maxResults ) {
            res += Sample( idx, m )
            resCnt += 1
         } else if( res.last.measure > m ) {
            res = res.dropRight( 1 ) + Sample( idx, m )
         }
      }

      if( numFrames > 0 ) {
         var x = 0; while( x < frameInteg ) {
            buf.getFrame( 0, frames.arr( x ))
         x += 1 }
         karlheinz( 0 )
      }
      var off = 1; while( off < numFrames ) {
//            val fm = frameMeasure( buf.getFrame( off, chanBuf ))
         if( rotateBuf ) {
            var y = 0; while( y < numChannels ) {
               var prev = frames.arr( 0 )( y )
               var x = frameIntegM; while( x >= 0 ) {   // ouch....
                  val tmp = frames.arr( x )( y )
                  frames.arr( x )( y ) = prev
                  prev = tmp
               x -= 1 }
            y += 1 }
            buf.getFrame( off, frames.arr( frameIntegM ))
         } else {
            buf.getFrame( off, frames.arr( (off - 1) % frameInteg ))
         }
         karlheinz( off )
      off += 1 }

      informDir( "searchAnalysisM done" )
      fun( res )
   }

   private def spawn( thunk: => Unit )( implicit tx: ProcTxn ) = afterCommit( tx ) { actor ! Do( thunk )}

   private object Do { def apply( thunk: => Unit ) = new Do( thunk )}
   private class Do( thunk: => Unit ) { def perform = thunk }
   case class Sample( idx: Int, measure: Float ) extends Ordered[ Sample ] {
       def compare( that: Sample ) : Int = idx.compare( that.idx )
   }
   private val sampleOrd = Ordering.ordered[ Sample ]

   case class State( valid: Boolean, thinking: Boolean = false, playing: Boolean = false )
   case class Update( p: Process, state: State )

   type Listener = TxnModel.Listener[ Process.Update ]
}

trait Process extends TxnModel[ Process.Update ] {
   proc =>

   import Process._

//   private val activeRef = Ref( false )
   private val stateRef = Ref( State( true ))

   def name : String
   def verbose : Boolean

   def init( implicit tx: ProcTxn ) : Unit
   final def stop( implicit tx: ProcTxn ) {
      val st = state
      if( st.valid ) state = st.copy( valid = false )
   }

   protected def keepGoing( implicit tx: ProcTxn ) = state.valid

   protected def inform( what: => String, force: Boolean = false )( implicit tx: ProcTxn ) = if( verbose || force ) {
      doInform( name + " : " + what )
   }

   protected def informDir( what: => String, force: Boolean = false )  = if( verbose || force ) {
      println( timeString + " " + name + " : " + what )
   }

   protected def startThinking( implicit tx: ProcTxn ) {
      val st = state
      if( !st.thinking ) state = st.copy( thinking = true )
   }

   protected def stopThinking( implicit tx: ProcTxn ) {
      val st = state
      if( st.thinking ) state = st.copy( thinking = false )
   }

   protected def startPlaying( implicit tx: ProcTxn ) {
      val st = state
      if( !st.playing ) state = st.copy( playing = true )
   }

   protected def stopPlaying( implicit tx: ProcTxn ) {
      val st = state
      if( st.playing ) state = st.copy( playing = false )
   }

   def state( implicit tx: ProcTxn ) = stateRef()
   private def state_=( newState: State )( implicit tx: ProcTxn ) {
      val oldState = stateRef.swap( newState )
      if( oldState != newState ) {
         touch
         val u = updateRef()
         updateRef.set( u.copy( state = newState ))
      }
   }

   protected def emptyUpdate  = Update( proc, State( false ))
   protected def fullUpdate( implicit tx: ProcTxn ) = Update( proc, state )
}
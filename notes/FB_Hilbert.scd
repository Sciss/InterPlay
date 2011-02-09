g.boot;
GUI.swing;

{ var x, y, z;
	x = SinOsc.ar( 666 ) * EnvGen.ar( Env.linen( 0.1, 0.1, 0.1 ));
	#y, z = Hilbert.ar( x );
	[ x, y, x.neg, z ]
}.plot( 0.3 )


//

gen( "fff" ) {
   val pamp = pControl( "amp", ParamSpec( 0.01, 1, ExpWarp ), 0.01 )
   val pfreq = pControl( "freq", ParamSpec( 0.5, 10, ExpWarp ), 2 )
   val plag = pControl( "lag", ParamSpec( 0.1, 1, ExpWarp ), 0.1 )
   graph {
       val sig = In.ar( NumOutputBuses.ir ) * pamp.kr
       // val Seq( hlbr, hlbi ) = Hilbert.ar( sig )
       val sigi = -sig
       XFade2.ar( sig, sigi, Lag.ar( LFPulse.kr( pfreq.kr ) * 2 - 1, plag.kr ))
   }
}

"::::"


gen( "hhh" ) {
   val pamp = pControl( "amp", ParamSpec( 0.01, 1, ExpWarp ), 0.01 )
   val pfreq = pControl( "freq", ParamSpec( 0.1, 10, ExpWarp ), 1 )
//   val plag = pControl( "lag", ParamSpec( 0.1, 1, ExpWarp ), 0.1 )
   graph {
       val sig = In.ar( NumOutputBuses.ir ) * pamp.kr
       val Seq( hlbr, hlbi ) = Hilbert.ar( sig ).outputs
//       val sigi = -sig
       val gugu = hlbr :: hlbi :: -hlbr :: -hlbi :: Nil
       val idx = LFSaw.kr( pfreq.kr ).madd( 2, 2 ) 
       val sig1: GE = gugu.zipWithIndex.map {
           case (ch, i) => ch * (1 - i.absdif( idx ).min( i.absdif( idx - 4 )))
       }
       sig1
   }
}


gen( "iii" ) {
   val pamp = pControl( "amp", ParamSpec( 0.01, 1, ExpWarp ), 0.01 )
   val pfreq = pControl( "freq", ParamSpec( 0.1, 10, ExpWarp ), 1 )
//   val plag = pControl( "lag", ParamSpec( 0.1, 1, ExpWarp ), 0.1 )
   graph {
       val sig = In.ar( NumOutputBuses.ir ) * pamp.kr
//       val Seq( hlbr, hlbi ) = Hilbert.ar( sig ).outputs
       val fftBuf = bufEmpty( 1024 ).id
       val chain = FFT( fftBuf, sig )
       val hlbr = IFFT( PV_PhaseShift90( chain ))
       val dly = BufDur.kr( fftBuf )
       val hlbi = DelayN.ar( sig, dly, dly )
       val gugu = hlbr :: hlbi :: -hlbr :: -hlbi :: Nil
       val idx = LFSaw.kr( pfreq.kr ).madd( 2, 2 ) 
       val sig1: GE = gugu.zipWithIndex.map {
           case (ch, i) => ch * (1 - i.absdif( idx ).min( i.absdif( idx - 4 )))
       }
       sig1
   }
}

gen( "jjj" ) {
   val pamp = pControl( "amp", ParamSpec( 0.01, 1, ExpWarp ), 0.01 )
   val pfreq = pControl( "freq", ParamSpec( 0.5, 10, ExpWarp ), 2 )
   val plag = pControl( "lag", ParamSpec( 0.1, 1, ExpWarp ), 0.1 )
   graph {
       val sig0 = In.ar( NumOutputBuses.ir ) * pamp.kr
       val sig = HPZ1.ar( sig0 )
       // val Seq( hlbr, hlbi ) = Hilbert.ar( sig )
       val sigi = -sig
       val flt = XFade2.ar( sig, sigi, Lag.ar( LFPulse.kr( pfreq.kr ) * 2 - 1, plag.kr ))
       LPZ1.ar( flt )
   }
}



gen( "kkk" ) {
   val pamp = pControl( "amp", ParamSpec( 0.01, 1, ExpWarp ), 0.01 )
   val pfreq = pControl( "freq", ParamSpec( 0.5, 10, ExpWarp ), 2 )
   val plag = pControl( "lag", ParamSpec( 0.1, 1, ExpWarp ), 0.1 )
   graph {
       val sig0 = In.ar( NumOutputBuses.ir ) * pamp.kr
       val sig = HPZ1.ar( sig0 ) * 2
       // val Seq( hlbr, hlbi ) = Hilbert.ar( sig )
       val sigi = -sig
       val flt = XFade2.ar( sig, sigi, Lag.ar( LFPulse.kr( pfreq.kr ) * 2 - 1, plag.kr ))
       Integrator.ar( flt, 0.995 )
   }
}


gen( "lll" ) {
   val pamp = pControl( "amp", ParamSpec( 0.01, 1, ExpWarp ), 0.01 )
   val pfreq = pControl( "freq", ParamSpec( 0.5, 10, ExpWarp ), 2 )
   val plag = pControl( "lag", ParamSpec( 0.1, 1, ExpWarp ), 0.1 )
   val pon = pControl( "on", ParamSpec( 0, 1, LinWarp, 1 ), 0 )
   graph {
       val sig0 = In.ar( NumOutputBuses.ir ) * pamp.kr
       val sig = HPZ1.ar( sig0 ) * 2
       // val Seq( hlbr, hlbi ) = Hilbert.ar( sig )
       val sigi = -sig
       val flt = XFade2.ar( sig, sigi, Lag.ar( LFPulse.kr( pfreq.kr ) * 2 - 1, plag.kr ))
       val flt1 = Integrator.ar( flt, 0.995 )
       Select.ar( pon.kr, sig0 :: flt1 :: Nil )
   }
}


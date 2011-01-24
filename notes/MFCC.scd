
//assumes hop of half fftsize, fine
b = Buffer.alloc(s,1024,1); //for sampling rates 44100 and 48000
//b = Buffer.alloc(s,2048,1); //for sampling rates 88200 and 96000

/// ---- live
(
x= {
var in, fft, array;

//in= PlayBuf.ar(1,d,BufRateScale.kr(d),1,0,1);

in = SoundIn.ar(0); 

fft = FFT(b, in);

array=MFCC.kr(fft); 

array.size.postln; 

Out.kr(0,array); 

// Out.ar(0,Pan2.ar(in)); 
0
}.play
)
/// ---- disk
p = "/Users/hhrutz/Desktop/InterPlay/rec/StringsDirect/StringsDirect1to4.aif";
d = Buffer.cueSoundFile( s, p, 0, 2, 32768 );
(
x= {
var in, fft, array;

//in= PlayBuf.ar(1,d,BufRateScale.kr(d),1,0,1);

in = Mix(DiskIn.ar(2,d.bufnum,1)); 

fft = FFT(b, in);

array=MFCC.kr(fft); 

array.size.postln; 

Out.kr(0,array); 

Out.ar(0,Pan2.ar(in)); 
}.play
)
// d.close; d.free


c= Bus.new('control', 0, 13); 

//poll coefficients
c.getn(13,{arg val; {val.plot;}.defer}); 



//Continuous graphical display of MFCC values; free routine before closing window

(
var ms, ok = true; 

w=Window.new("Thirteen MFCC coefficients", Rect(200,400,300,300));

ms= MultiSliderView.new(w, Rect(10,10,260,280));

ms.value_(Array.fill(13,0.0));
ms.valueThumbSize_(20.0);
ms.indexThumbSize_(20.0);
ms.gap_(0);

w.front;

r= {

while({ ok }, {

c.getn(13,{arg val; {
	if( ms.isClosed, {
		ok = false;
	}, {
		ms.value_(val*0.6);
	});
}.defer}); 

0.04.wait; //25 frames per second
});

}.fork;

)
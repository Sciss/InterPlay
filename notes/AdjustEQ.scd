~inBuf = Buffer.cueSoundFile( s, "/Users/hhrutz/Desktop/InterPlay/audio_work/work/swp11'659CtConsweepRvs.aif", 0, 1 );
~outBuf = Buffer.alloc( s, 32768, 1 );
~outBuf.write( "/Users/hhrutz/Desktop/InterPlay/audio_work/work/_killme.aif", sampleFormat: "float", numFrames: 0, leaveOpen: true );

// (
{ arg inBuf, outBuf, dur = 4.5;
x = DiskIn.ar( 1, inBuf );
Line.kr( dur: dur, doneAction: 2 );
x = MidEQ.ar( x, 382, 5.reciprocal, -3 );
x = MidEQ.ar( x, 706, 10.reciprocal, -3 );
x = MidEQ.ar( x, 799, 13.reciprocal, -3 );
x = MidEQ.ar( x, 929, 11.reciprocal, -6 );
x = MidEQ.ar( x, 1333, 8.reciprocal, -6 );
x = BHiShelf.ar( x, 5500, 1, 6 );
DiskOut.ar( outBuf, x );
0.0;
}.play( args: [ \inBuf, ~inBuf.bufnum, \outBuf, ~outBuf.bufnum ]);
// )

~outBuf.close; ~outBuf.free; ~inBuf.close; ~inBuf.free;

382 -3
706 -3
799 -3
929 -6
1333 -4.5
2000 shelf

(418-340/382).reciprocal
(736-670/706).reciprocal
(835-776/799).reciprocal
(950-866/929).reciprocal
(1386-1233/1333).reciprocal

s.options.device="MOTU 828mk2";
s.options.numInputBusChannels=22;
s.reboot;

// (
~includeSweep = true;
~numChans = 6;
~amp = 0.05;
~sweepDur = 20;
~rvbDur = 5;
~f0 = 20;
~f1 = 20000;
~outOff = 2;
~inOff = 0;
~folder = "~/Desktop/InterPlay/rec/sweep".standardizePath;

SynthDef( \sweep, { arg sweepDur, rvbDur, f0, f1, amp, recBuf, inOff, outOff;
	var mic 	= In.ar( NumOutputBuses.ir + inOff, 1 );
	var freq 	= Line.ar( f0, f1, sweepDur );
	var env	= EnvGen.ar( Env([ 1, 1, 0 ], [ sweepDur, rvbDur ], \step ), doneAction: 2 );
	var sweep= SinOsc.ar( freq ) * env;
	var rec  = if( ~includeSweep, {
	   [ mic, sweep ]
	}, {
	   [ mic ]
	});
//	env.poll(1);
	DiskOut.ar( recBuf, rec );
	Out.ar( outOff, (sweep  * amp) ! ~numChans );
}).send( s );

// )

// (
f = Routine({ var bndl, buf, synth, newMsg;
"...Prepare".postln;
	6.wait;
"...Go".postln;
	buf = Buffer( s, 32768, if( ~includeSweep, 2, 1 ));
	synth = Synth.basicNew( \sweep, s );
	newMsg = synth.newMsg( s, [ \sweepDur, ~sweepDur, \rvbDur, ~rvbDur, \f0, ~f0, \f1, ~f1, \amp, ~amp, \recBuf, buf.bufnum, \inOff, ~inOff, \outOff, ~outOff ]);
	buf.alloc( buf.writeMsg( ~folder +/+ "sweep" ++ Date.getDate.stamp ++ ".aif", sampleFormat: "float", numFrames: 0, leaveOpen: true, completionMessage: newMsg ));
	synth.waitForEnd;
"...Done".postln;
	buf.close; buf.free;
}).play( SystemClock );
// )

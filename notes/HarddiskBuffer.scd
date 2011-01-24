b = Buffer.alloc( s, 32768, 1 );
p = "~/Desktop/diskout.irc".standardizePath;
b.write( p, "ircam", "float", 0, 0, true );

x = { DiskOut.ar( b.bufnum, SoundIn.ar( 0 )); Silent.ar }.play;

y = SoundFile.openRead( p );
y.close;
y.numFrames

c = Buffer.cueSoundFile( s, p, 0, 1, 32768 );
c = Buffer.cueSoundFile( s, p, 44100 * 120, 1, 32768 );
z = { DiskIn.ar( 1, c.bufnum )}.play;

// problem nur: numFrames von c wird beim cueing fixiert,
// d.h. stattdessen wird TapeUnit maessiges b_read cycling
// benoetigt, um mit dem fortlaufend geschriebenen soundfile
// mitzukommen...

z.free;
c.close; c.free;

//
x.free;
b.close; b.free

## Inter-Play / Re-Sound

Software for a piece for derelict piano, computer and human perfomer. (C)opyright 2011&ndash;2012 by Hanns Holger Rutz. All rights reserved. Software covered by the GNU General Public License v2+ (see licenses folder).

### Dependencies

 - Builds with sbt 0.7 against Scala 2.8.1.
 - SuperCollider 3.4.4 -- __don't__ use SC 3.5 beta
 - Wolkenpumpe 0.23 / SoundProcesse 0.22 / FScapeJobs 0.11

Use `sbt_old package-app` to re-create the OS X app bundle (assuming `sbt_old` launches sbt 0.7).

### Configuration

In `Interplay`

 - `INTERNAL_AUDIO`     -- `true` for testing on internal sound card, `false` for concert and MOTU
 - `MASTER_OFFSET`      -- channel offset for first master output. Should be `2` (aka analog 1 on MOTU)
 - `MASTER_NUMCHANNELS` -- for concert probably `6`. for internal testing can be set to `2`.
 - `MIC_OFFSET`         -- where the DPA is connected. Should be `0`.
 - `MIC_NUMCHANNELS`    -- should be `1`.
 - `BASE_PATH`          -- main audio data folder
 - `USE_MIDI`           -- whether the BCF 2000 is connected. Safe to leave on `true`.
 - `AUTO_RECORD`        -- whether to make recordings automatically when the piece is started. Leave at `true` to avoid mayhem.
 - `LIVE_FILE`          -- `Some( micFile )` for testing, otherwise `None`.
 - `INITIAL_MASTER_VOLUME` -- self-explanatory. Default is `2.0.
 - `inDevice` / `outDevice` -- defaults to `"MOTU 828mk2"`.

Note: templates are found in `BASE_PATH/templates`. Live mic recording goes to `BASE_PATH/rec/live`. Live output recording goes to `BASE_PATH/rec/mitschnitt`.

In `SoundProcesses`:

 - `ONSET_THRESH`       -- detection setting for onsets. Default is `0.5`
 - `liveDur`            -- duration for recording the performer in __minutes__. Should be between `2.5` and `3.5`
 - `totalDur`           -- approximate (logical) duration in __minutes__. Should be between `6.5` and `8.5`.
 - `LIVE_AMP_SPEC`      -- min/max gain for microphone signal which is read from BCF fader

In `Midi`:

 - `IN_DESCR`, `OUT_DESCR` -- MIDI port names. Default to `"BCF2000 Port 1"`.
 - `MIC_CHAN`           -- MIDI channel used for the fader controlling the live input signal (i.e. via `SoundProcesses. LIVE_AMP_SPEC`). Defaults to `7` (the last faster on the BCF in a typical preset). You can see the fader value in Wolkenpumpe in the `O-live` proc.
 - `MAST_CHAN`		-- MIDI channel used for the fader controlling master volume. Doesn't seem to have visual feedback with version of Wolkenpumpe used?? Typical `6` or `5` with UoP's broken 7th motor fader.
 - `PLAY_BUT`   -- MIDI CC (as seen on channel 0) used to start the piece (when CC > 0). `89` for the top left button in the BCF's button section in the bottom right.
 - `STOP_BUT`   -- MIDI CC (as seen on channel 0) used to stop the piece (when CC == 0). `92` for the bottom right button in the BCF's button section in the bottom right.
 - `DUMP_IN`, `DUMP_OUT` -- can be enabled to test the midi connection.

### MIDI

A few parameters are controllable with a BCF 2000:

### Channels

Outputs: With the default of 6 channels, these are : front centre, front right, right, left, top, front left

### Things to Remember

 - At launch FScape must be ready and receiving OSC

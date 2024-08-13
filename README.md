
Re-truncated Daggerfall MIDI files by looking for two consecutive Note On with same pitch but velocity 1 then 0, on each track, and truncating the track at the previous event. Remove later Note On, clamp all further event to last timestamp (pulse).

That's by no meant an "ultimate" approach, it would certainly be better to check the original HMI files for full information; Knowing that HMI looping specs have never been fully reverse-engineered though.



To get non manually truncated MIDI files from Daggerfall Unity repository:

$ git checkout 'bec807dbae4f36ae413bec4424532c31a505254c^'
$ cd Assets/Resources/Songs


let max_tracks = 64

type midi_event =
  | Header of int * int * int
  | TitleT of string
  | MarkerT of string
  | TimeSignature of int * int * int * int
  | KeySignature of int * string
  | InstrumentNameT of string
  | ChannelPrefix of int
  | StartTrack
  | Tempo of int
  | ControlC of int * int * int
  | ProgramC of int * int
  | NoteOnC of int * int * int
  | NoteOffC of int * int * int
  | PitchBendC of int * int
  | SystemExclusive of int list
  | UnknownMetaEvent of int list
  | SMPTEOffset of int * int * int * int * int
  | MIDIPort of int
  | EndTrack
  | EndOfFile

type full_event = {
    track: int;
    pulses: int;
    event: midi_event
  }

let parse_midi_event = function
  | ["Header"; a; b; c] -> Some (Header (int_of_string a, int_of_string b, int_of_string c))
  | ["Title_t"; a] -> Some (TitleT a)
  | ["Marker_t"; a] -> Some (MarkerT a)
  | ["Time_signature"; a; b; c; d] -> Some (TimeSignature (int_of_string a, int_of_string b, int_of_string c, int_of_string d))
  | ["Key_signature"; a; b] -> Some (KeySignature (int_of_string a, b))
  | ["Instrument_name_t"; a] -> Some (InstrumentNameT a)
  | ["Channel_prefix"; a] -> Some (ChannelPrefix (int_of_string a))
  | ["Start_track"] -> Some StartTrack
  | ["Tempo"; t] -> Some (Tempo (int_of_string t))
  | ["Control_c"; a; b; c] -> Some (ControlC (int_of_string a, int_of_string b, int_of_string c))
  | ["Program_c"; a; b] -> Some (ProgramC (int_of_string a, int_of_string b))
  | ["Note_on_c"; a; b; c] -> Some (NoteOnC (int_of_string a, int_of_string b, int_of_string c))
  | ["Note_off_c"; a; b; c] -> Some (NoteOffC (int_of_string a, int_of_string b, int_of_string c))
  | ["Pitch_bend_c"; a; b] -> Some (PitchBendC (int_of_string a, int_of_string b))
  | "System_exclusive" :: list -> Some (SystemExclusive (List.map int_of_string list))
  | "Unknown_meta_event" :: list -> Some (UnknownMetaEvent (List.map int_of_string list))
  | ["SMPTE_offset"; a; b; c; d; e] -> Some (SMPTEOffset (int_of_string a, int_of_string b, int_of_string c, int_of_string d, int_of_string e))
  | ["MIDI_port"; a] -> Some (MIDIPort (int_of_string a))
  | ["End_track"] -> Some EndTrack
  | ["End_of_file"] -> Some EndOfFile
  | _ -> None

let midi_event_to_string = function
  | Header (a,b,c) -> Printf.sprintf "Header, %d, %d, %d" a b c
  | TitleT a -> Printf.sprintf "Title_t, %s" a
  | MarkerT a -> Printf.sprintf "Marker_t, %s" a
  | TimeSignature (a,b,c,d) -> Printf.sprintf "Time_signature, %d, %d, %d, %d" a b c d
  | KeySignature (a,b) -> Printf.sprintf "Key_signature, %d, %s" a b
  | InstrumentNameT a -> Printf.sprintf "Instrument_name_t, %s" a
  | ChannelPrefix a -> Printf.sprintf "Channel_prefix, %d" a
  | StartTrack -> "Start_track"
  | Tempo t -> Printf.sprintf "Tempo, %d" t
  | ControlC (a,b,c) -> Printf.sprintf "Control_c, %d, %d, %d" a b c
  | ProgramC (a,b) -> Printf.sprintf "Program_c, %d, %d" a b
  | NoteOnC (a,b,c) -> Printf.sprintf "Note_on_c, %d, %d, %d" a b c
  | NoteOffC (a,b,c) -> Printf.sprintf "Note_off_c, %d, %d, %d" a b c
  | PitchBendC (a,b) -> Printf.sprintf "Pitch_bend_c, %d, %d" a b
  | SystemExclusive l -> Printf.sprintf "System_exclusive, %s"
                           (String.concat ", " (List.map string_of_int l))
  | UnknownMetaEvent l -> Printf.sprintf "Unknown_meta_event, %s"
                            (String.concat ", " (List.map string_of_int l))
  | SMPTEOffset (a,b,c,d,e) -> Printf.sprintf "SMPTE_offset, %d, %d, %d, %d, %d" a b c d e
  | MIDIPort a -> Printf.sprintf "MIDI_port, %d" a
  | EndTrack -> "End_track"
  | EndOfFile -> "End_of_file"
       
let print_event event =
  Printf.printf "%d, %d, %s\n"
    event.track
    event.pulses
    (midi_event_to_string event.event)

let read_tracks filename =
  let tracks = Array.init (max_tracks + 1) (fun _ -> []) in
  let chan = open_in filename in
  try
    let comma = Str.regexp ", " in
    while true; do
      let line = input_line chan in
      let fields = Str.split comma line in
      match fields with
      | track :: pulses :: rest ->
         let midi_event = parse_midi_event rest in
         (match midi_event with
          | Some midi_event ->
             let event = {
                 track = int_of_string track;
                 pulses = int_of_string pulses;
                 event = midi_event
               } in
             (* print_event event *)
             (match event.event with
              | EndOfFile -> ()
              | _ ->
                 tracks.(event.track) <- event :: tracks.(event.track))
          | None ->
             Printf.eprintf "Unknown event [%s]\n" (String.concat "; " rest))
      | _ ->
         Printf.eprintf "Don't know how to parse [%s]\n" (String.concat "; " fields)
    done;
    failwith "Should never be reached"
  with End_of_file ->
    close_in chan;
    Array.map_inplace List.rev tracks;
    tracks

let print_tracks tracks =
  for track = 0 to max_tracks do
    List.iter print_event tracks.(track)
  done;
  print_event { track = 0; pulses = 0; event = EndOfFile }

(* Look for the first 2 consecutive NoteOnC of same pitch and of velocity 1 and 0 respectively;
   Return the pulses of the last event before them
 *)
let track_length track =
  let rec aux last_pulse track =
    match track with
    | { event = NoteOnC (_, pitch1, 1); _ } ::
        { event = NoteOnC (_, pitch2, 0); _ } ::
          _ when pitch1 = pitch2 -> last_pulse
    | { pulses; _ } :: rest -> aux pulses rest
    | [] -> last_pulse in
  aux 0 track

let truncate_track track_number track =
  let length = track_length track in
  let new_track =
    List.filter_map (function
        (* keep everything until `length` pulses, included *)
        | { pulses; _ } as event when pulses <= length -> Some event
        (* remove all NoteOnC after that *)
        | { event = NoteOnC (_); _ } -> None
        (* for other events, clap pulses to length *)
        | event -> Some { event with pulses = length }
      ) track in
  let new_percentage = 100. *. float (List.length new_track) /. float (List.length track) in
  if new_percentage < 95. then
    Printf.eprintf "Warning: track %d is %f.0%% of original track length\n" track_number new_percentage;
  new_track
  
let () =
  let tracks = read_tracks Sys.argv.(1) in
(*  let track_lengths = Array.map track_length tracks in
    let track_length = Array.fold_left max 0 track_lengths in *)
  let truncated_tracks = Array.mapi truncate_track tracks in
  print_tracks truncated_tracks

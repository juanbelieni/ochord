open OChord

let () =
  match Parser.parse (Array.get Sys.argv 1) with
  | Ok chord ->
      print_endline @@ "Chord: " ^ Chord.to_string chord;
      print_endline @@ "Notes: "
      ^ String.concat ", "
          (List.map
             (fun (note, octave) ->
               Note.to_string note ^ Printf.sprintf "[o=%d]" octave)
             (Chord.notes chord));
      print_endline @@ Piano.generate_from_notes (Chord.notes chord)
  | Error msg -> failwith msg

open OChord

let () =
  match Parser.parse (Array.get Sys.argv 1) with
  | Ok chord -> print_endline @@ Piano.generate_from_notes (Chord.notes chord)
  | Error msg -> failwith msg

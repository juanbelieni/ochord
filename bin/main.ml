open OChord

let () =
  match Parser.parse (Array.get Sys.argv 1) with
  | Ok expression ->
      let notes = Parser.get_notes expression in
      print_endline @@ Piano.generate_from_notes notes
  | Error msg -> failwith msg

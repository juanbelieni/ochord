open Angstrom

let note_name =
  choice
    (List.map
       (fun note -> string @@ Note.to_string @@ note)
       [ Note.A; Note.B; Note.C; Note.D; Note.E; Note.F; Note.G ])
  >>| fun name -> Note.from_string name

let note : Note.t t =
  lift2
    (fun note accidental ->
      match accidental with
      | 'b' -> Note.flat note
      | '#' -> Note.sharp note
      | str -> failwith ("Unexpected accidental: " ^ Char.escaped str))
    note_name
    (char 'b' <|> char '#')
  <|> note_name

let third : Chord.interval t =
  char 'M'
  <|> (char 'm'
      <* (peek_char >>= function
          | Some 'a' -> fail ""
          | c -> return c))
  >>| (function
        | 'M' -> Chord.MajorThird
        | 'm' -> Chord.MinorThird
        | c -> failwith ("Unexpected char: " ^ Char.escaped c))
  <|> return Chord.MajorThird

let fifth : Chord.interval t =
  char '('
  *> (string "b5" <|> string "#5" >>| function
      | "b5" -> Chord.DiminishedFifth
      | "#5" -> Chord.AugmentedFifth
      | str -> failwith ("Unexpected string: " ^ str))
  <* char ')' <|> return Chord.PerfectFifth

let seventh : Chord.interval option t =
  string "6" <|> string "7M" <|> string "7" <|> string "maj7"
  >>| (function
        | "6" -> Some Chord.Sixth
        | "7" -> Some Chord.MinorSeventh
        | "7M" -> Some Chord.MajorSeventh
        | "maj7" -> Some Chord.MajorSeventh
        | str -> failwith ("Unexpected string: " ^ str))
  <|> return None

let chord : Chord.t t =
  lift4
    (fun root third fifth seventh : Chord.t ->
      { root; intervals = third :: fifth :: Option.to_list seventh })
    note third fifth seventh

let parse str = parse_string ~consume:Consume.All chord str

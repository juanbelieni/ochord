open Angstrom

let note_name : Note.name t =
  lift2
    (fun base_name accidental ->
      let (note : Note.t) =
        match accidental with
        | Some 'b' -> Note.from_name base_name |> Note.flat
        | Some '#' -> Note.from_name base_name |> Note.sharp
        | Some c -> failwith ("Accidental not supported: " ^ Char.escaped c)
        | None -> Note.from_name base_name
      in
      note.name)
    (Note.name_list
    |> List.map (fun name -> Note.name_to_string @@ name)
    |> List.filter (fun name -> String.length name == 1)
    |> List.map string |> choice >>| Note.name_from_string)
    (option Option.none (char 'b' <|> char '#' >>| Option.some))

let third : Chord.interval t =
  char 'm'
  <* (peek_char >>= function
      | Some 'a' -> fail ""
      | c -> return c)
  >>| (function
        | 'm' -> Chord.MinorThird
        | c -> failwith ("Unexpected char: " ^ Char.escaped c))
  <|> return Chord.MajorThird

let fifth =
  string "b5" *> return [ Chord.DiminishedFifth ]
  <|> string "#5" *> return [ Chord.AugmentedFifth ]

let seventh =
  string "6" *> return [ Chord.Sixth ]
  <|> string "7" *> return [ Chord.MinorSeventh ]
  <|> string "7M" *> return [ Chord.MajorSeventh ]
  <|> string "M7" *> return [ Chord.MajorSeventh ]
  <|> string "maj7" *> return [ Chord.MajorSeventh ]
  <|> string "Maj7" *> return [ Chord.MajorSeventh ]
  <|> string "9" *> return [ Chord.MinorSeventh; Chord.Ninth ]

let ninth =
  string "b9" *> return [ Chord.MinorNinth ]
  <|> string "#9" *> return [ Chord.MinorNinth ]

let params =
  char '['
  *> sep_by (char ',')
       (lift3
          (fun name _ value ->
            ( String.of_seq @@ List.to_seq name,
              String.of_seq @@ List.to_seq value ))
          (many1 (not_char '='))
          (char '=')
          (many1 (satisfy (fun c -> c <> ',' && c <> ']'))))
  <* char ']' <|> return []

let chord : Chord.t t =
  (fun root third seventh fifth ninth params : Chord.t ->
    { root; intervals = third :: List.concat [ seventh; fifth; ninth ]; params })
  <$> note_name <*> third
  <*> (seventh <|> return [])
  <*> (char '(' *> fifth <* char ')' <|> return [ Chord.PerfectFifth ])
  <*> (char '(' *> ninth <* char ')' <|> return [])
  <*> ( params >>| fun list ->
        List.fold_left
          (fun _params param : Chord.params ->
            match param with
            | "o", str -> { octave = int_of_string str }
            | name, _ -> failwith ("Unexpected chord param: " ^ name))
          Chord.default_params list )

let note : Note.t t =
  (fun note_name (params : Note.params) : Note.t ->
    { name = note_name; params })
  <$> char '.' *> note_name
  <*> ( params >>| fun list ->
        List.fold_left
          (fun _params param : Note.params ->
            match param with
            | "o", str -> { octave = int_of_string str }
            | name, _ -> failwith ("Unexpected chord param: " ^ name))
          Note.default_params list )

type expression =
  | Note of Note.t
  | Chord of Chord.t
  | Union of expression list

let withespace =
  skip_while (function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false)

let expression : expression t =
  sep_by
    (withespace *> char '+' <* withespace)
    (chord >>| (fun chord -> Chord chord) <|> (note >>| fun note -> Note note))
  >>| fun exprs -> Union exprs

let parse str = parse_string ~consume:Consume.All expression str

let rec get_notes = function
  | Note note -> [ note ]
  | Chord chord -> Chord.get_notes chord
  | Union exprs -> List.concat_map (fun expr -> get_notes expr) exprs

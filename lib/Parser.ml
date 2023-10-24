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

let fifth : Chord.interval t =
  char '('
  *> (string "b5" <|> string "#5" >>| function
      | "b5" -> Chord.DiminishedFifth
      | "#5" -> Chord.AugmentedFifth
      | str -> failwith ("Unexpected string: " ^ str))
  <* char ')' <|> return Chord.PerfectFifth

let seventh : Chord.interval option t =
  string "6" <|> string "7M" <|> string "M7" <|> string "7" <|> string "maj7"
  <|> string "Maj7"
  >>| (function
        | "6" -> Some Chord.Sixth
        | "7" -> Some Chord.MinorSeventh
        | "7M" -> Some Chord.MajorSeventh
        | "M7" -> Some Chord.MajorSeventh
        | "maj7" -> Some Chord.MajorSeventh
        | "Maj7" -> Some Chord.MajorSeventh
        | str -> failwith ("Unexpected string: " ^ str))
  <|> return None

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
  (fun root third fifth seventh params : Chord.t ->
    { root; intervals = third :: fifth :: Option.to_list seventh; params })
  <$> note_name <*> third <*> fifth <*> seventh
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

let withespace = many @@ char ' '

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

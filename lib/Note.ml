type name =
  | C
  | Db
  | D
  | Eb
  | E
  | F
  | Gb
  | G
  | Ab
  | A
  | Bb
  | B

let name_list = [ C; Db; D; Eb; E; F; Gb; G; Ab; A; Bb; B ]

let name_to_string = function
  | C -> "C"
  | Db -> "Db"
  | D -> "D"
  | Eb -> "Eb"
  | E -> "E"
  | F -> "F"
  | Gb -> "Gb"
  | G -> "G"
  | Ab -> "Ab"
  | A -> "A"
  | Bb -> "Bb"
  | B -> "B"

let name_from_string = function
  | "C" -> C
  | "Db" -> Db
  | "D" -> D
  | "Eb" -> Eb
  | "E" -> E
  | "F" -> F
  | "Gb" -> Gb
  | "G" -> G
  | "Ab" -> Ab
  | "A" -> A
  | "Bb" -> Bb
  | "B" -> B
  | str -> failwith ("Unexpected note: " ^ str)

type params = { octave : int }

let default_params = { octave = 0 }

type t = {
  name : name;
  params : params;
}

let from_name name = { name; params = default_params }

let list_index note =
  List.find_index
    (fun name -> name_to_string note.name = name_to_string name)
    name_list
  |> Option.get

let flat note =
  match list_index note with
  | 0 -> { name = B; params = { octave = note.params.octave - 1 } }
  | i -> { note with name = List.nth name_list (i - 1) }

let sharp note =
  match list_index note with
  | 11 -> { name = C; params = { octave = note.params.octave + 1 } }
  | i -> { note with name = List.nth name_list (i + 1) }

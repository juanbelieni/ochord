type t =
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

let list = [ C; Db; D; Eb; E; F; Gb; G; Ab; A; Bb; B ]

let to_string = function
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

let from_string = function
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

let list_index note =
  List.find_index (fun note' -> to_string note = to_string note') list
  |> Option.get

let flat note =
  match list_index note with
  | 0 -> B
  | i -> List.nth list (i - 1)

let sharp note =
  match list_index note with
  | 11 -> C
  | i -> List.nth list (i + 1)

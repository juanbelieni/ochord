type interval =
  | MinorThird
  | MajorThird
  | DiminishedFifth
  | PerfectFifth
  | AugmentedFifth
  | Sixth
  | MinorSeventh
  | MajorSeventh

let semitones interval =
  match interval with
  | MinorThird -> 3
  | MajorThird -> 4
  | DiminishedFifth -> 6
  | PerfectFifth -> 7
  | AugmentedFifth -> 8
  | Sixth -> 9
  | MinorSeventh -> 10
  | MajorSeventh -> 11

type t = {
  root : Note.t;
  intervals : interval list;
}

let to_string { root; intervals } =
  Note.to_string root
  ^ List.fold_left ( ^ ) ""
      (List.map
         (function
           | MinorThird -> "m"
           | MajorThird -> ""
           | DiminishedFifth -> "(b5)"
           | PerfectFifth -> ""
           | AugmentedFifth -> "(#5)"
           | Sixth -> "6"
           | MinorSeventh -> "7"
           | MajorSeventh -> "7M")
         intervals)

let notes { root; intervals } : (Note.t * int) list =
  let rec sharp_n note n =
    match (note, n) with
    | note, 0 -> note
    | note, n -> sharp_n (Note.sharp note) (n - 1)
  in
  (root, 0)
  :: List.map
       (fun interval ->
         let semitones = semitones interval in
         (sharp_n root semitones, Int.div (Note.list_index root + semitones) 12))
       intervals
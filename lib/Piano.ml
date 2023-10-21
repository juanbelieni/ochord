let _piano_octave =
  "   Db  Eb      Gb  Ab  Bb    \n\
   ┌──┬─┬─┬─┬──┬──┬─┬─┬─┬─┬─┬──┐\n\
   │00│1│2│3│44│55│6│7│8│9│@│##│\n\
   │00│1│2│3│44│55│6│7│8│9│@│##│\n\
   │00│1│2│3│44│55│6│7│8│9│@│##│\n\
   │00└┬┘2└┬┘44│55└┬┘7└┬┘9└┬┘##│\n\
   │000│222│444│555│777│999│###│\n\
   │000│222│444│555│777│999│###│\n\
   └───┴───┴───┴───┴───┴───┴───┘\n\
  \  C   D   E   F   G   A   B  "

let _piano_char_from_note = function
  | Note.C -> '0'
  | Note.Db -> '1'
  | Note.D -> '2'
  | Note.Eb -> '3'
  | Note.E -> '4'
  | Note.F -> '5'
  | Note.Gb -> '6'
  | Note.G -> '7'
  | Note.Ab -> '8'
  | Note.A -> '9'
  | Note.Bb -> '@'
  | Note.B -> '#'

let _clean_piano piano =
  Str.global_replace (Str.regexp "[0123456789@#]") " " piano

let _concat_octaves piano =
  piano
  |> Str.global_replace (Str.regexp "Bb       Db") "Bb      Db"
  |> Str.global_replace (Str.regexp "┐┌") "┬"
  |> Str.global_replace (Str.regexp "││") "│"
  |> Str.global_replace (Str.regexp "┘└") "┴"
  |> Str.global_replace (Str.regexp "B    C") "B   C"

let generate_from_notes (notes : (Note.t * int) list) =
  let rec generate notes octave piano =
    match notes with
    | (note, octave') :: notes' ->
        let piano, octave =
          if octave' > octave then
            ( List.combine
                (_clean_piano piano |> String.split_on_char '\n')
                (_piano_octave |> String.split_on_char '\n')
              |> List.map (fun (s1, s2) -> s1 ^ s2)
              |> String.concat "\n",
              octave + 1 )
          else (piano, octave)
        in
        if octave = octave' then
          generate notes' octave
            (Str.global_replace
               (Str.regexp (Char.escaped (_piano_char_from_note note)))
               "⣿" piano)
        else generate notes octave piano
    | [] -> piano |> _clean_piano |> _concat_octaves
  in
  generate notes 0 _piano_octave

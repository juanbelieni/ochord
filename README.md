# OChord

A terminal-based interface to manipulate chords and notes.

## Features

### Visualizer

A chord can be visualized on a piano by specifying it as the first argument of the application:

```sh
./ochord "<chord expression>"
```

The chord expression can be specified:
- As just a note (e.g.: "Db"), being interpreted as a major triad.
- With information about the third (e.g.: "Dbm").
- With information about the fifth (e.g.: "Db(b5)" or "Db(#5)").
- With information about other intervals (e.g.: "(Db7)"). NOTE: the major seventh interval can be specified as "M7", "7M", "maj7" or "Maj7".
- With extra params (e.g.: "Db[o=1]"):
  - octave (`o`): number of octaves that all the notes but the root will be raised.
  - root octave (`r`): number of octaves that the root note will be raised.

## References

- https://en.wikipedia.org/wiki/Chord_notation
- https://en.wikipedia.org/wiki/Jazz_chord

## License

This project is distributed under the [MIT License](LICENSE.md).

## Piano

```
   Db  Eb      Gb  Ab  Bb
┌──┬─┬─┬─┬──┬──┬─┬─┬─┬─┬─┬──┐
│  │ │ │ │  │  │ │ │ │ │ │  │
│  │ │ │ │  │  │ │ │ │ │ │  │
│  │ │ │ │  │  │ │ │ │ │ │  │
│  └┬┘ └┬┘  │  └┬┘ └┬┘ └┬┘  │
│   │   │   │   │   │   │   │
│   │   │   │   │   │   │   │
└───┴───┴───┴───┴───┴───┴───┘
  C   D   E   F   G   A   B
```

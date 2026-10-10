# multi-paradigm_problem_OCaml

Command-line program written in OCaml (functional style) that reads a CSV file, lets you pick one numeric column, and prints descriptive statistics for it. It is the OCaml part of a multi-paradigm assignment (C, OCaml, Python) that computes mean, median and mode.

## Build

```sh
dune build
```

This compiles `bin/main.ml` into the `statistics` executable. `dune clean` removes the `_build/` folder. Requires OCaml and dune (`brew install ocaml dune` or `opam install dune`).

## Run

```sh
dune exec statistics -- test.csv
```

The CSV must be comma-separated with a header row. The program lists the numeric columns of the file:

```text
Select one option to return data:

1: task_id
2: worker_id
...
15: fare
16: duration_ms

Choose an option:
```

Type the number of a column and press Enter.

## What to expect

For the chosen column the program prints the statistics of its values, sorted internally:

```text
Median: 17.930000
Sum: 366136.940000
Mean: 18.306847
Mode: 7.580000 17.090000 17.930000
Q1: 12.380000
Q2: 17.930000
Q3: 23.740000
IQR: 11.360000
Population variance: 56.380645
Population std dev: 7.508705
Sample variance: 56.383464
Sample std dev: 7.508892
Min: 2.580000
Max: 42.060000
Range: 39.480000
```

- Integer columns (e.g. `task_id`) are handled as integers, decimal columns (e.g. `fare`) as doubles.
- Median of an even count is the average of the two middle values.
- Mode prints every value tied for most frequent, smallest first; if every value is unique it reports that there is no mode.
- Quartiles use linear interpolation between ranks; IQR is Q3 - Q1.

## Layout

- `bin/main.ml`: the whole program in one file, in sections (types, checking cells, reading the file, columns, statistics, main)

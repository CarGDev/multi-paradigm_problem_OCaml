(* Statistics of one numeric column of a CSV file.
   Usage: dune exec statistics -- file.csv

   Functional style: no mutable state and no loops. Values flow through
   pure functions (map, filter_map, fold) and recursion. Only reading the
   file, asking the user and printing touch the outside world. *)

(* ---------- Types ---------- *)

(* a numeric column: its name, its position in the row, and if it is integer *)
type column = { name : string; index : int; is_int : bool }

(* binds a result to the next step, stopping at the first error *)
let ( let* ) = Result.bind

(* ---------- Checking cells ---------- *)

(* true if the character is a digit *)
let is_digit c = c >= '0' && c <= '9'

(* true if the text is an integer, like 12 or -7 *)
let is_integer text =
  let n = String.length text in
  n > 0
  &&
  let start = if text.[0] = '-' || text.[0] = '+' then 1 else 0 in
  n > start && String.for_all is_digit (String.sub text start (n - start))

(* true if the text is a decimal number, like 3.14 *)
let is_decimal text =
  text <> ""
  && (not (String.contains text '_'))
  && Option.is_some (float_of_string_opt text)

(* ---------- Reading the file ---------- *)

(* removes the \r and \n at the end of a text *)
let rec remove_newline text =
  let n = String.length text in
  if n > 0 && (text.[n - 1] = '\n' || text.[n - 1] = '\r') then
    remove_newline (String.sub text 0 (n - 1))
  else text

(* splits a line by commas. A comma inside quotes does not split.
   The state is (finished fields, current field reversed, inside quotes). *)
let split_line line =
  let step (fields, current, in_quotes) c =
    if c = '"' then (fields, current, not in_quotes)
    else if c = ',' && not in_quotes then
      (String.of_seq (List.to_seq (List.rev current)) :: fields, [], in_quotes)
    else (fields, c :: current, in_quotes)
  in
  let fields, current, _ =
    String.fold_left step ([], [], false) line
  in
  let last = remove_newline (String.of_seq (List.to_seq (List.rev current))) in
  List.rev (last :: fields)

(* reads all the lines of the file into a list *)
let read_lines path =
  match open_in path with
  | exception Sys_error _ -> Error "statistics: Invalid file."
  | file ->
      let rec loop acc =
        match input_line file with
        | line -> loop (line :: acc)
        | exception End_of_file -> List.rev acc
      in
      let lines = loop [] in
      close_in file;
      Ok lines

(* ---------- Columns ---------- *)

(* the column described by a name and a cell, if the cell is a number *)
let column_of index (name, cell) =
  if is_integer cell then Some { name; index; is_int = true }
  else if is_decimal cell then Some { name; index; is_int = false }
  else None

(* pairs each name with its cell; names without a cell are dropped *)
let rec zip names cells =
  match (names, cells) with
  | name :: names, cell :: cells -> (name, cell) :: zip names cells
  | _ -> []

(* finds the numeric columns, looking at the first row of data *)
let find_columns header_line first_line =
  zip (split_line header_line) (split_line first_line)
  |> List.mapi column_of
  |> List.filter_map Fun.id

(* shows the columns and asks the user for one. Returns the chosen column,
   or None when the input ends without a valid choice. *)
let choose_column columns =
  let total = List.length columns in
  print_string "Select one option to return data:\n\n";
  List.iteri (fun i c -> Printf.printf "%d: %s\n" (i + 1) c.name) columns;
  let rec ask () =
    print_string "\nChoose an option: ";
    flush stdout;
    match input_line stdin with
    | exception End_of_file -> None
    | "end" -> None
    | line -> (
        let text = String.trim line in
        match if is_integer text then int_of_string_opt text else None with
        | Some n when n >= 1 && n <= total -> Some (List.nth columns (n - 1))
        | _ ->
            Printf.printf "Invalid option, enter a number between 1 and %d.\n"
              total;
            ask ())
  in
  ask ()

(* the number in the cell of the column, if the cell has the right type *)
let value_of column line =
  match List.nth_opt (split_line line) column.index with
  | Some cell when column.is_int && is_integer cell ->
      Some (float_of_string cell)
  | Some cell when (not column.is_int) && is_decimal cell ->
      Some (float_of_string cell)
  | _ -> None

(* gets the numbers of the column from all the data lines, sorted *)
let get_values lines column =
  lines |> List.filter_map (value_of column) |> List.sort compare |> Array.of_list

(* ---------- Statistics ---------- *)

(* a number as text: integers without decimals *)
let show column value =
  if column.is_int then Printf.sprintf "%d" (int_of_float value)
  else Printf.sprintf "%f" value

(* the middle value. With an even count, the average of the two in the middle *)
let median_line column values =
  let n = Array.length values in
  if n mod 2 = 1 then Printf.sprintf "Median: %s" (show column values.(n / 2))
  else
    Printf.sprintf "Median: %f" ((values.(n / 2 - 1) +. values.(n / 2)) /. 2.0)

let sum values = Array.fold_left ( +. ) 0.0 values

(* sum and mean *)
let sum_and_mean_lines values =
  let total = sum values in
  [
    Printf.sprintf "Sum: %f" total;
    Printf.sprintf "Mean: %f" (total /. float_of_int (Array.length values));
  ]

(* groups equal neighbours: [1;1;2] becomes [(1, 2); (2, 1)].
   The values are sorted, so equal values are together. *)
let runs values =
  Array.fold_left
    (fun acc v ->
      match acc with
      | (x, count) :: rest when x = v -> (x, count + 1) :: rest
      | _ -> (v, 1) :: acc)
    [] values
  |> List.rev

(* the value (or values) that appear more times *)
let mode_line column values =
  let groups = runs values in
  let best = List.fold_left (fun m (_, count) -> max m count) 1 groups in
  if best = 1 then "Mode: there is no mode"
  else
    groups
    |> List.filter (fun (_, count) -> count = best)
    |> List.map (fun (v, _) -> " " ^ show column v)
    |> String.concat ""
    |> ( ^ ) "Mode:"

(* percentile p (from 0 to 100) using a line between the two closest values *)
let percentile values p =
  let n = Array.length values in
  let rank = p /. 100.0 *. float_of_int (n - 1) in
  let low = int_of_float rank in
  let high = if low + 1 < n then low + 1 else low in
  let fraction = rank -. float_of_int low in
  values.(low) +. (fraction *. (values.(high) -. values.(low)))

let quartiles_lines values =
  let q1 = percentile values 25.0 in
  let q2 = percentile values 50.0 in
  let q3 = percentile values 75.0 in
  [
    Printf.sprintf "Q1: %f" q1;
    Printf.sprintf "Q2: %f" q2;
    Printf.sprintf "Q3: %f" q3;
    Printf.sprintf "IQR: %f" (q3 -. q1);
  ]

(* population variance divides by n, sample variance divides by n - 1 *)
let variance_lines values =
  let n = float_of_int (Array.length values) in
  let mean = sum values /. n in
  let squares =
    Array.fold_left (fun acc v -> acc +. ((v -. mean) *. (v -. mean))) 0.0 values
  in
  let population = squares /. n in
  let sample_lines =
    if Array.length values < 2 then [ "Sample variance: needs at least 2 values" ]
    else
      let sample = squares /. (n -. 1.0) in
      [
        Printf.sprintf "Sample variance: %f" sample;
        Printf.sprintf "Sample std dev: %f" (sqrt sample);
      ]
  in
  [
    Printf.sprintf "Population variance: %f" population;
    Printf.sprintf "Population std dev: %f" (sqrt population);
  ]
  @ sample_lines

(* the array is sorted: the smallest is the first, the biggest is the last *)
let min_max_lines column values =
  let smallest = values.(0) in
  let biggest = values.(Array.length values - 1) in
  [
    Printf.sprintf "Min: %s" (show column smallest);
    Printf.sprintf "Max: %s" (show column biggest);
    Printf.sprintf "Range: %f" (biggest -. smallest);
  ]

(* all the lines of the report, in order *)
let report column values =
  (median_line column values :: sum_and_mean_lines values)
  @ (mode_line column values :: quartiles_lines values)
  @ variance_lines values
  @ min_max_lines column values

(* ---------- Main ---------- *)

(* runs the whole program. Error carries the message and the exit code. *)
let run argv =
  let* path =
    if Array.length argv <> 2 then Error ("Only one argument is allowed", 1)
    else Ok argv.(1)
  in
  let* lines = Result.map_error (fun msg -> (msg, 1)) (read_lines path) in
  (* we need the header line and at least one line of data *)
  let* header_line, first_line, data =
    match lines with
    | header_line :: first_line :: data -> Ok (header_line, first_line, data)
    | _ -> Error ("statistics: no data", 1)
  in
  let* columns =
    match find_columns header_line first_line with
    | [] -> Error ("statistics: no data", 1)
    | columns -> Ok columns
  in
  let* column =
    Option.to_result
      ~none:("statistics: no numeric column selected", 1)
      (choose_column columns)
  in
  match get_values (first_line :: data) column with
  | [||] -> Error ("statistics: the column has no valid values", 0)
  | values -> Ok (report column values)

let () =
  match run Sys.argv with
  | Ok lines -> List.iter print_endline lines
  | Error (message, code) ->
      prerr_endline message;
      if code <> 0 then exit code

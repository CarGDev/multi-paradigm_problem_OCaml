(* Statistics of one numeric column of a CSV file.
   Usage: dune exec statistics -- file.csv *)

(* ---------- Types ---------- *)

(* a numeric column: its name, its position in the row, and if it is integer *)
type column = { name : string; index : int; is_int : bool }

(* ---------- Checking cells ---------- *)

(* true if the character is a digit *)
let is_digit c = c >= '0' && c <= '9'

(* true if the text is an integer, like 12 or -7 *)
let is_integer text =
  let n = String.length text in
  if n = 0 then false
  else begin
    let start = if text.[0] = '-' || text.[0] = '+' then 1 else 0 in
    let ok = ref (n > start) in
    for i = start to n - 1 do
      if not (is_digit text.[i]) then ok := false
    done;
    !ok
  end

(* true if the text is a decimal number, like 3.14 *)
let is_decimal text =
  if text = "" || String.contains text '_' then false
  else
    try
      let _ = float_of_string text in
      true
    with Failure _ -> false

(* ---------- Reading the file ---------- *)

(* removes the \r and \n at the end of a text *)
let remove_newline text =
  let n = ref (String.length text) in
  while !n > 0 && (text.[!n - 1] = '\n' || text.[!n - 1] = '\r') do
    n := !n - 1
  done;
  String.sub text 0 !n

(* splits a line by commas. A comma inside quotes does not split. *)
let split_line line =
  let fields = ref [] in
  let current = ref "" in
  let in_quotes = ref false in
  String.iter
    (fun c ->
      if c = '"' then in_quotes := not !in_quotes
      else if c = ',' && not !in_quotes then begin
        fields := !current :: !fields;
        current := ""
      end
      else current := !current ^ String.make 1 c)
    line;
  fields := remove_newline !current :: !fields;
  Array.of_list (List.rev !fields)

(* reads all the lines of the file into a list *)
let read_lines path =
  let file = open_in path in
  let lines = ref [] in
  (try
     while true do
       lines := input_line file :: !lines
     done
   with End_of_file -> ());
  close_in file;
  List.rev !lines

(* ---------- Columns ---------- *)

(* finds the numeric columns, looking at the first row of data *)
let find_columns header_line first_line =
  let names = split_line header_line in
  let cells = split_line first_line in
  let columns = ref [] in
  for i = 0 to Array.length names - 1 do
    if i < Array.length cells then begin
      if is_integer cells.(i) then
        columns := { name = names.(i); index = i; is_int = true } :: !columns
      else if is_decimal cells.(i) then
        columns := { name = names.(i); index = i; is_int = false } :: !columns
    end
  done;
  Array.of_list (List.rev !columns)

(* shows the columns and asks the user for one. Returns the chosen column. *)
let choose_column columns =
  let total = Array.length columns in
  print_string "Select one option to return data:\n\n";
  for i = 0 to total - 1 do
    Printf.printf "%d: %s\n" (i + 1) columns.(i).name
  done;
  let chosen = ref (-1) in
  while !chosen < 0 do
    print_string "\nChoose an option: ";
    flush stdout;
    let line = try input_line stdin with End_of_file -> "end" in
    if line = "end" then begin
      prerr_endline "statistics: no numeric column selected";
      exit 1
    end;
    let text = String.trim line in
    if is_integer text && int_of_string text >= 1 && int_of_string text <= total
    then chosen := int_of_string text - 1
    else Printf.printf "Invalid option, enter a number between 1 and %d.\n" total
  done;
  columns.(!chosen)

(* gets the numbers of the column from all the data lines, sorted *)
let get_values lines column =
  let values = ref [] in
  List.iter
    (fun line ->
      let cells = split_line line in
      if column.index < Array.length cells then begin
        let cell = cells.(column.index) in
        if column.is_int && is_integer cell then
          values := float_of_string cell :: !values
        else if (not column.is_int) && is_decimal cell then
          values := float_of_string cell :: !values
      end)
    lines;
  let array = Array.of_list !values in
  Array.sort compare array;
  array

(* ---------- Statistics ---------- *)

(* a number as text: integers without decimals *)
let show column value =
  if column.is_int then Printf.sprintf "%d" (int_of_float value)
  else Printf.sprintf "%f" value

(* the middle value. With an even count, the average of the two in the middle *)
let print_median column values =
  let n = Array.length values in
  if n mod 2 = 1 then Printf.printf "Median: %s\n" (show column values.(n / 2))
  else begin
    let average = (values.(n / 2 - 1) +. values.(n / 2)) /. 2.0 in
    Printf.printf "Median: %f\n" average
  end

(* sum and mean *)
let print_sum_and_mean values =
  let sum = ref 0.0 in
  for i = 0 to Array.length values - 1 do
    sum := !sum +. values.(i)
  done;
  Printf.printf "Sum: %f\n" !sum;
  Printf.printf "Mean: %f\n" (!sum /. float_of_int (Array.length values))

(* the value (or values) that appear more times.
   The array is sorted, so equal values are together. *)
let print_mode column values =
  let n = Array.length values in
  (* first, find the biggest group of equal values *)
  let best = ref 1 in
  let i = ref 0 in
  while !i < n do
    let j = ref !i in
    while !j < n && values.(!j) = values.(!i) do
      j := !j + 1
    done;
    if !j - !i > !best then best := !j - !i;
    i := !j
  done;
  if !best = 1 then print_endline "Mode: there is no mode"
  else begin
    (* then print every group that has that size *)
    print_string "Mode:";
    i := 0;
    while !i < n do
      let j = ref !i in
      while !j < n && values.(!j) = values.(!i) do
        j := !j + 1
      done;
      if !j - !i = !best then Printf.printf " %s" (show column values.(!i));
      i := !j
    done;
    print_newline ()
  end

(* percentile p (from 0 to 100) using a line between the two closest values *)
let percentile values p =
  let n = Array.length values in
  let rank = p /. 100.0 *. float_of_int (n - 1) in
  let low = int_of_float rank in
  let high = if low + 1 < n then low + 1 else low in
  let fraction = rank -. float_of_int low in
  values.(low) +. (fraction *. (values.(high) -. values.(low)))

let print_quartiles values =
  let q1 = percentile values 25.0 in
  let q2 = percentile values 50.0 in
  let q3 = percentile values 75.0 in
  Printf.printf "Q1: %f\n" q1;
  Printf.printf "Q2: %f\n" q2;
  Printf.printf "Q3: %f\n" q3;
  Printf.printf "IQR: %f\n" (q3 -. q1)

(* population variance divides by n, sample variance divides by n - 1 *)
let print_variance values =
  let count = Array.length values in
  let n = float_of_int count in
  let sum = ref 0.0 in
  for i = 0 to count - 1 do
    sum := !sum +. values.(i)
  done;
  let mean = !sum /. n in
  let squares = ref 0.0 in
  for i = 0 to count - 1 do
    let diff = values.(i) -. mean in
    squares := !squares +. (diff *. diff)
  done;
  let population = !squares /. n in
  Printf.printf "Population variance: %f\n" population;
  Printf.printf "Population std dev: %f\n" (sqrt population);
  if count < 2 then print_endline "Sample variance: needs at least 2 values"
  else begin
    let sample = !squares /. (n -. 1.0) in
    Printf.printf "Sample variance: %f\n" sample;
    Printf.printf "Sample std dev: %f\n" (sqrt sample)
  end

(* the array is sorted: the smallest is the first, the biggest is the last *)
let print_min_max column values =
  let smallest = values.(0) in
  let biggest = values.(Array.length values - 1) in
  Printf.printf "Min: %s\n" (show column smallest);
  Printf.printf "Max: %s\n" (show column biggest);
  Printf.printf "Range: %f\n" (biggest -. smallest)

(* ---------- Main ---------- *)

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Only one argument is allowed";
    exit 1
  end;
  let lines =
    try read_lines Sys.argv.(1)
    with Sys_error _ ->
      prerr_endline "statistics: Invalid file.";
      exit 1
  in
  (* we need the header line and at least one line of data *)
  match lines with
  | header_line :: first_line :: data ->
      let columns = find_columns header_line first_line in
      if Array.length columns = 0 then begin
        prerr_endline "statistics: no data";
        exit 1
      end;
      let column = choose_column columns in
      let values = get_values (first_line :: data) column in
      if Array.length values = 0 then
        prerr_endline "statistics: the column has no valid values"
      else begin
        print_median column values;
        print_sum_and_mean values;
        print_mode column values;
        print_quartiles values;
        print_variance values;
        print_min_max column values
      end
  | _ ->
      prerr_endline "statistics: no data";
      exit 1

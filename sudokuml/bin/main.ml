let rec slice list s e = if s = e then [] else (List.nth list s) :: slice list (s+1) e


let rec range min max = 
  if min >= max then [] else min :: range (min+1) max;;


let rec get_box board r c counter = if r = counter then [] else slice (List.nth board r) c (c+3)


let find_box_nums board col row =
  let col_start = (col / 3) * 3 in 
  let row_start = (row / 3) * 3 in
  let box = get_box board col_start row_start (col_start+3) in
  List.filter (fun x -> not (List.mem x box)) (range 1 10)


let find_row_nums board col = 
  List.filter (fun x -> not (List.mem x board.(col))) (range 1 10)


let find_col_nums board row = 
  let col = List.map (fun n -> board.(n).(row)) (range 0 9) in
  List.filter (fun x -> not (List.mem x col)) (range 1 10)


let list_to_array list = 
  Array.of_list (List.map Array.of_list list)


let read_board file_name = 
  let ic = open_in file_name in
    let rec read_line ic =
      try
        let line_str = input_line ic in
        let line = List.map (fun x -> Char.code x - 48) (List.init (String.length line_str) (String.get line_str)) in
        line :: read_line ic
      with End_of_file ->
        close_in ic;
        []
  in
  read_line ic
 

let () =
  let board = read_board "./bin/game.txt" in
  Array.iter (fun row -> Array.iter (fun col -> print_int col) row; print_newline ()) (list_to_array board)
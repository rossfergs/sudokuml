let _print_int_list list = Array.iter (fun x -> print_int x) (Array.of_list list)


let list_to_array list = 
  Array.of_list (List.map Array.of_list list)


let _print_board board = 
  Array.iter (fun row -> Array.iter (fun col -> print_int col) row; print_newline ()) (list_to_array board)


let rec slice list s e = 
  if s = e 
    then [] 
  else (List.nth list s) :: slice list (s+1) e


let rec range min max = 
  if min >= max then [] else min :: range (min+1) max;;


let rec get_box board c r counter = 
  if c = counter 
    then [] 
  else List.append (slice (List.nth board c) r (r+3)) (get_box board (c+1) r counter)


let find_box_nums board col row =
  let col_start = (col / 3) * 3 in 
  let row_start = (row / 3) * 3 in
  let box = get_box board col_start row_start (col_start+3) in
  List.filter (fun x -> not (List.mem x box)) (range 1 10)


let find_row_nums board col = 
  List.filter (fun x -> not (List.mem x (List.nth board col))) (range 1 10)


let find_col_nums board row = 
  let col = List.map (fun n -> List.nth (List.nth board n) row) (range 0 9) in
  List.filter (fun x -> not (List.mem x col)) (range 1 10)


let combine_lists l1 l2 l3 =
  let sorted_uniques = (List.sort_uniq compare (List.append (List.append (l1) (l2)) (l3))) in 
  List.filter 
    (fun n -> List.mem n l1 && List.mem n l2 && List.mem n l3)
    sorted_uniques 


let find_numbers board col row = 
  let col_nums = find_col_nums board row in
  let row_nums = find_row_nums board col in
  let box_nums = find_box_nums board col row in 
  Array.of_list (combine_lists col_nums row_nums box_nums)


let rec solve board col row = 
  if (col >= 8 && row >= 9) then board
  else (
    if row >= 9 then solve board (col+1) 0 
    else if List.nth (List.nth board col) row != 0 then solve board col (row+1) 
    else (
      let numbers = find_numbers board col row in 
      let rec find_solved_board board col row nums idx = 
        if (idx >= Array.length nums) then [] 
        else 
          let new_board = 
            List.mapi (fun i l -> if i = col 
              then (List.mapi (fun j n -> if j = (row-1) then nums.(idx) else n) (List.nth board col)) 
              else l) board 
          in 
          let result = solve new_board col row in
          if result != [] then result
          else find_solved_board board col row nums (idx+1)
      in
      find_solved_board board col (row+1) numbers 0
    )
  )


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
  let solved = solve board 0 0 in
  let () = _print_board solved in
  if solved = [] then (print_int 0) else (print_int 1)
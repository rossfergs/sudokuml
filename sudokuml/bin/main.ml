

let list_to_array list = 
  Array.of_list (List.map Array.of_list list)

let read_board file_name = 
  let list_board =
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
  in list_to_array list_board
 

let () =
  let board = read_board "./bin/game.txt" in
  Array.iter (fun row -> Array.iter (fun col -> print_int col) row; print_newline ()) board
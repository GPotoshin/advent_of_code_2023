let rec extract_part lst start length =
  match (lst, start, length) with
  | (_, 0, 0) -> []
  | ([], _, _) -> []
  | (_, 0, len) -> List.hd lst :: extract_part (List.tl lst) 0 (len - 1)
  | (_, _, _) -> extract_part (List.tl lst) (start - 1) length
;;


let second_problem file =
    let rec read_card list_of_counters =
        (* let rec print_l l = 
            match l with
            | n :: r -> Printf.printf "%d " n; print_l r
            | _ -> print_char '\n';
        in print_l list_of_counters; *)
        try
        let line = input_line file in
        let sp_line = String.split_on_char ' ' line in
        let ft_line = List.filter (fun x -> x <> "") sp_line in
        let sh_list = match ft_line with
        | _ :: _ :: rest -> rest
        | _ -> [] in
        let winning_numbers =
            let rec read_winning_numbers nums =
                match nums with
                | n :: r ->
                        if n = "|" then []
                        else (int_of_string n) :: read_winning_numbers r
                | _ -> []
            in
            read_winning_numbers sh_list
        in
        let numbers_you_have =
            let rec read_numbers_you_have nums =
                match nums with
                | n :: r ->
                        if n = "|" then read_numbers_you_have r
                        else (int_of_string n) :: read_numbers_you_have r
                | _ -> []
            in
            read_numbers_you_have (extract_part sh_list 
            (List.length winning_numbers + 1) 
            (List.length sh_list - List.length winning_numbers - 1))
        in
        let is_winning x = List.mem x winning_numbers in
        let count_of_card = List.length
            (List.filter is_winning numbers_you_have) in
        let number_of_counters = List.length list_of_counters in
        let decreased_list_of_counters =
            let rec decrease = fun x -> match x with 
            | n :: r -> (n-1) :: decrease r | _ -> [] in
            decrease list_of_counters in
        let concat_list_of_c = List.concat [decreased_list_of_counters;
            List.init (1+number_of_counters) (fun _ -> count_of_card)] in
        let filtered_l_of_c = List.filter (fun x -> x > 0) concat_list_of_c in

        1 + number_of_counters + read_card filtered_l_of_c
        with | End_of_file -> 0
    in
    print_int (read_card []);
    print_char '\n'
;;


let first_problem file = 
    let rec read_card () =
        try
        let line = input_line file in
        let sp_line = String.split_on_char ' ' line in
        let ft_line = List.filter (fun x -> x <> "") sp_line in
        let sh_list = match ft_line with
        | _ :: _ :: rest -> rest
        | _ -> [] in
        let winning_numbers =
            let rec read_winning_numbers nums =
                match nums with
                | n :: r ->
                        if n = "|" then []
                        else (int_of_string n) :: read_winning_numbers r
                | _ -> []
            in
            read_winning_numbers sh_list
        in
        let numbers_you_have =
            let rec read_numbers_you_have nums =
                match nums with
                | n :: r ->
                        if n = "|" then read_numbers_you_have r
                        else (int_of_string n) :: read_numbers_you_have r
                | _ -> []
            in
            read_numbers_you_have (extract_part sh_list 
            (List.length winning_numbers + 1) 
            (List.length sh_list - List.length winning_numbers - 1))
        in
        let is_winning x = List.mem x winning_numbers in
        (int_of_float (2. ** float_of_int (List.length (List.filter is_winning numbers_you_have))))/2
        + read_card ()
        with | End_of_file -> 0
    in
    print_int (read_card ());
    print_char '\n'
;;


let () =
    let file = open_in "input.txt" in
    (* first_problem file *)
    second_problem file
;;

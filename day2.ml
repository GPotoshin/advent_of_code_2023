let first_problem file =
    let rec game_is_correct = fun list ->
        let color = match list with | _ :: s :: _ -> s | _ -> "" in
        let num = match list with | n :: _ -> int_of_string n | _ -> 0 in
        let sublist = match list with
            | _ :: _ :: rest -> rest
            | _ -> [] in
        match color with
        | "red;" -> if num > 12 then false else
            game_is_correct (sublist)
        | "red," -> if num > 12 then false else
            game_is_correct (sublist)
        | "red" -> if num > 12 then false else
            game_is_correct (sublist)
        | "green;" -> if num > 13 then false else
            game_is_correct (sublist)
        | "green," -> if num > 13 then false else
            game_is_correct (sublist)
        | "green" -> if num > 13 then false else
            game_is_correct (sublist)
        | "blue;" -> if num > 14 then false else
            game_is_correct (sublist)
        | "blue," -> if num > 14 then false else
            game_is_correct (sublist)
        | "blue" -> if num > 14 then false else
            game_is_correct (sublist)
        | _ -> true
    in
    let rec sum_correct = fun file sum ->
        try
        let line = input_line file in
        let sp_line = String.split_on_char ' ' line in
        let sh_list = match sp_line with
            | _ :: _ :: rest -> rest
            | _ -> [] in
        match sp_line with
            | _ :: num :: _ ->
                if game_is_correct sh_list
                then
                sum_correct file (sum + (int_of_string 
                (String.sub num 0 (String.length num - 1))))
                else sum_correct file sum
            | _ -> -1
        with | End_of_file -> sum
    in
    print_int (sum_correct file 0);
    print_char '\n'
;;

let second_problem file =
    let rec power_of = fun list r g b->
        let color = match list with | _ :: s :: _ -> s | _ -> "" in
        let n = match list with | x :: _ -> int_of_string x | _ -> 0 in
        let sublist = match list with
            | _ :: _ :: rest -> rest
            | _ -> [] in
        match color with
        | "red;" ->
                power_of sublist (max r n) g b
        | "red," ->
                power_of sublist (max r n) g b
        | "red" ->
                power_of sublist (max r n) g b
        | "green;" ->
                power_of sublist r (max g n) b
        | "green," ->
                power_of sublist r (max g n) b
        | "green" ->
                power_of sublist r (max g n) b
        | "blue;" ->
                power_of sublist r g (max b n)
        | "blue," ->
                power_of sublist r g (max b n)
        | "blue" ->
                power_of sublist r g (max b n)
        | _ -> r*g*b
    in
    let rec sum_powers = fun file sum ->
        try
            let line = input_line file in
            let sp_line = String.split_on_char ' ' line in
            let sh_list = match sp_line with
                | _ :: _ :: rest -> rest
                | _ -> [] in
            sum_powers file (sum + power_of sh_list 0 0 0)
        with | End_of_file -> sum
    in

    print_int (sum_powers file 0);
    print_char '\n'
;;

let () =
    let file = open_in "input.txt" in
    (* first_problem file *)
    second_problem file
;;

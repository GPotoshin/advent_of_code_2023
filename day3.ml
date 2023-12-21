let second_problem file =
    let all_lines =
        let rec read_lines acc =
            try
                let line = input_line file in
                read_lines (line :: acc)
            with | End_of_file -> close_in file;
            List.rev acc in
        read_lines []
        in
    let zero_line =  "............................................................................................................................................" in
    let rec pr_line line pos prev_line next_line sum =
        if String.length line = pos then
            sum
        else
            let ch = line.[pos] in
            match ch with
            | '*' ->
                    let rec accumulate_back l i str =
                        try
                            match '0' <= l.[i] && l.[i] <= '9' with
                            | true -> 
                                accumulate_back l (i-1) ((String.make 1 l.[i])^str)
                            | false -> str
                        with | Invalid_argument _ -> str
                    in

                    let rec accumulate_front l i str = 
                        try
                            match '0' <= l.[i] && l.[i] <= '9' with
                            |true ->
                                accumulate_front l (i+1) (str^(String.make 1 l.[i]))
                            | false -> str
                        with | Invalid_argument _ -> str
                    in

                    let c1 = 0 in

                    let back = int_of_string (
                        match accumulate_back line (pos-1) "" with
                        | "" -> "0" | s -> s) in
                    let c2 = c1 + match back with | 0 -> 0 | _ -> 1 in

                    let front = int_of_string (
                        match accumulate_front line (pos+1) "" with
                        | "" -> "0" | s -> s) in
                    let c3 = c2 + match front with | 0 -> 0 | _ -> 1 in

                    let (upper_left, upper_right) =
                        if '0' <= prev_line.[pos] && prev_line.[pos] <= '9' then
                            (int_of_string (
                                match (accumulate_back prev_line pos "")^
                            (accumulate_front prev_line (pos+1) "")
                            with | "0" -> "" | s -> s), 0)
                        else
                            (int_of_string (
                                match accumulate_back prev_line (pos-1) "" with
                                | "" -> "0" | s -> s),
                            int_of_string (
                                match accumulate_front prev_line (pos+1) "" with
                                | "" -> "0" | s -> s))
                    in 
                    let c4 = c3 + match upper_left with | 0 -> 0 | _ -> 1 in
                    let c5 = c4 + match upper_right with | 0 -> 0 | _ -> 1 in


                    let (lower_left, lower_right) =
                        if '0' <= next_line.[pos] && next_line.[pos] <= '9' then
                            (int_of_string (
                                match (accumulate_back next_line pos "")^
                            (accumulate_front next_line (pos+1) "")
                            with |"" -> "0" | s -> s), 0)
                        else
                            (int_of_string (
                                match accumulate_back next_line (pos-1) "" with
                                | "" -> "0" | s -> s),
                            int_of_string (
                                match accumulate_front next_line (pos+1) "" with
                                | "" -> "0" | s -> s))
                    in 
                    let c6 = c5 + match lower_left with | 0 -> 0 | _ -> 1 in
                    let c7 = c6 + match lower_right with | 0 -> 0 | _ -> 1 in
                    
                    Printf.printf "b: %d, f: %d, up_l: %d, up_r: %d, l_l: %d, l_r: %d\n" back front upper_left upper_right lower_left lower_right;


                    if 2 = c7 then pr_line line (pos+1) prev_line next_line
                        (sum
                        + back*front + back*upper_left + back*upper_right + back*lower_left + back*lower_right
                        + front*upper_left + front*upper_right + front*lower_left + front*lower_right
                        + upper_left*upper_right + upper_left*lower_left + upper_left*lower_right
                        + upper_right*lower_left + upper_right*lower_right
                        + lower_left*lower_right)
                    else pr_line line (pos+1) prev_line next_line sum
            | _ -> pr_line line (pos+1) prev_line next_line sum
    in


    let rec process lines prev_line sum =
        Printf.printf "<--- line: %d, sum: %d --->\n" (List.length all_lines - List.length lines) sum;
        match lines with
        | line :: _ ->
                let next_line = match lines with
                | _ :: s :: _ -> s
                | _ -> zero_line in

        let new_sum = sum + pr_line line 0 prev_line next_line 0 in
        process (match lines with | _ :: rest -> rest | _ -> [])
        line new_sum
        | [] -> sum
        in

    print_int (process all_lines zero_line 0);
    print_char '\n'
;;





let first_problem file = 
    let all_lines =
        let rec read_lines acc =
            try
                let line = input_line file in
                read_lines (line :: acc)
                with | End_of_file -> close_in file; List.rev acc
                in
        read_lines []
        in
    let rec process lines prev_ind sum = 
        (* Printf.printf "<---- line:%d sum:%d ---->\n" (1 + List.length all_lines - List.length lines) sum; *)
        match lines with
        | line :: _ -> 
                let next_line = match lines with
                | _ :: s :: _ -> s
                | _ -> "............................................................................................................................................" in

                let (new_ind, new_sum) =
                    let rec pr_line num pos valid ind line_sum =
                        (* Printf.printf "%s -+- %B -+- sum %d\n" num valid line_sum; *)
                        if pos = String.length line then
                            if valid then
                                (ind, line_sum + (int_of_string num))
                else
                    (ind, line_sum)
                            else
                                let ch = line.[pos] in
                                if '0' <= ch && ch <= '9' then
                                    pr_line (num^(String.make 1 ch)) (pos+1)
                                    (valid || (List.mem pos prev_ind) ||
                                (next_line.[pos] != '.' &&
                                (next_line.[pos] > '9' ||
                                next_line.[pos] < '0')))
                                    ind line_sum
                        else if (valid || (List.mem pos prev_ind) ||
                                        (next_line.[pos] != '.' &&
                                        (next_line.[pos] > '9' ||
                                        next_line.[pos] < '0')) || ch != '.') then
                                            pr_line "0" (pos+1) ((List.mem pos prev_ind) ||
                                    (next_line.[pos] != '.' &&
                                    (next_line.[pos] > '9' ||
                                    next_line.[pos] < '0')) || ch != '.')
                                            (match ch with | '.' -> ind | _ -> pos :: ind)
                                            (line_sum + (int_of_string num))
                                else
                                    pr_line "0" (pos+1) ((List.mem pos prev_ind) ||
                                (next_line.[pos] != '.' &&
                                (next_line.[pos] > '9' ||
                                next_line.[pos] < '0')) || ch != '.')
                                    (match ch with | '.' -> ind | _ -> pos :: ind)
                                    (line_sum)
                                in pr_line "" 0 false [] 0
                    in
                process (match lines with | _ :: rest -> rest | _ -> []) new_ind (sum+new_sum)
                | _ -> sum
                in

    print_int (process all_lines [] 0);
    print_char '\n'
;;


let () =
    let file = open_in "input.txt" in
    (*first_problem file*)
    second_problem file
;;

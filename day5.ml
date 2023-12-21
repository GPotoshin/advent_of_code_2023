let compare_int = 
    (fun n1 n2 -> if n1 < n2 then -1
    else if n1 > n2 then 1
    else 0)
;;

let first_problem file = 
    let process seeds_line file = 
        let nums = List.sort compare_int seeds_line in

        let _ = input_line file in

        let rec map_numbers numbers =
            try
                let _ = input_line file in
                let lines =
                    let rec read_lines () =
                        try
                            match input_line file with
                | "" -> [] | s -> s :: read_lines ()
                            with | End_of_file -> [] in
                    read_lines () in
                let data =
                    List.sort (fun l1 l2 ->
                        let e1 = match l1 with | _ :: e :: _ -> e | _ -> -1 in
                        let e2 = match l2 with | _ :: e :: _ -> e | _ -> -1 in
                        if e1 < e2 then -1
                else if e1 > e2 then 1
                        else 0)
                    (List.map (fun line ->
                        List.map (fun str -> int_of_string str)
                        (String.split_on_char ' ' line)) lines) in

                let rec map_number ns d = 
                    match ns with
            | n :: rn ->
                    (match d with
                | l :: rd ->
                        (match l with
                    | s1 :: s2 :: len :: _ ->
                            if n < s2 then
                                n :: map_number rn d
                else if n < s2 + len then
                    (s1 + n - s2) :: map_number rn d
                            else
                                map_number ns rd
                    | _ -> [])
                        | _ -> n :: map_number rn d)
                    | _ -> [] 
                in

        let news = map_number numbers data in
        map_numbers (List.sort compare_int news)
                        with | End_of_file -> numbers
        in
    print_int (match map_numbers nums with | e :: _ -> e | _ -> -1);
    print_char '\n'
        in
    let seeds_line = 
        match String.split_on_char ' ' (input_line file) with
        | n :: r -> r | _ -> [] in
    process (List.map int_of_string seeds_line) file
;;

let second_problem file =
    let seeds_line =
        match String.split_on_char ' ' (input_line file) with
        | n :: r -> r | _ -> [] in
    let seeds_numbers = List.map (fun x -> int_of_string x) seeds_line in
    let seeds_intervals =
        let rec into_pairs list = 
            match list with
            | a :: b :: r -> [a; b] :: into_pairs r
            | _ -> [] in
        into_pairs seeds_numbers in
    let sorted_seeds = List.sort (fun a b ->
        let x = match a with | f :: _ -> f | _ -> -1 in
        let y = match b with | f :: _ -> f | _ -> -1 in
        if x > y then 1
        else if x < y then -1
        else 0) seeds_intervals
    in

    let _ = input_line file in

    let rec map_intervals intervals =
        try
            let rec print_intervals is =
                match is with | i :: ris ->
                    (match i with | a :: b :: _ ->
                        Printf.printf "[start: %d, len: %d]\n" a b;
                        print_intervals ris
                    | _ -> ());
                | _ -> () in
            print_endline "<------- intervals -------->";
            print_intervals intervals;
            let _ = input_line file in
            let lines =
                let rec get_data () = 
                    try
                    (match input_line file with
                    | "" -> [] | s -> s :: get_data ())
                    with | End_of_file -> []
                in get_data ()
            in
            let data = List.map (fun line ->
                let line_by_words = String.split_on_char ' ' line in
                List.map (fun x -> int_of_string x) line_by_words) lines
            in
            let sorted_data = List.sort (fun a b ->
                let x = match a with | _ :: n :: _ -> n | _ -> -1 in
                let y = match b with | _ :: n :: _ -> n | _ -> -1 in
                if x > y then 1
                else if x < y then -1
                else 0) data
            in
            let rec print_data d =
                match d with | pd :: rd ->
                    (match pd with | a :: b :: c :: _ ->
                        Printf.printf "[dest: %d, source: %d, range: %d]\n" a b c;
                        print_data rd
                    | _ -> ());
                | _ -> () in
            print_endline "<-------- data --------->";
            print_data sorted_data;

            let new_intervals =
                let rec parse_intervals is d =
                    match is with
                    | i :: ris ->
                    (match i with
                    | start :: irange :: _ ->
                    (match d with
                    | pd :: rd ->
                    (match pd with
                    | dest :: source :: drange :: _ ->
                        if start+irange < source then
                            i :: parse_intervals ris d
                        else if start < source then
                            [start; source-start] :: parse_intervals
                            ([source; irange-source+start] :: ris) d
                        else if start+irange < source+drange then
                            [dest+start-source; irange] ::
                            parse_intervals ris d
                        else if start < source+drange then
                            [dest+start-source; drange-start+source] ::
                            parse_intervals ([source+drange;
                            irange-(source+drange-start)] :: ris) rd
                        else parse_intervals is rd
                    | _ -> is)
                    | _ -> is)
                    | _ -> [])
                    | _ -> []
                in
                parse_intervals intervals sorted_data
            in
            let sorted_intervals = List.sort (fun a b ->
                let x = match a with | f :: _ -> f | _ -> -1 in
                let y = match b with | f :: _ -> f | _ -> -1 in
                if x > y then 1
                else if x < y then -1
                else 0) new_intervals
            in
            map_intervals sorted_intervals
        with | End_of_file -> intervals
    in
print_int (match map_intervals sorted_seeds with
    | f :: _ -> (match f with p :: _ -> p | _ -> -2) | _ -> -1);
    print_char '\n'
;;

let () =
    let file = open_in "input.txt" in
    second_problem file
;;

let parse_data s =
    match List.filter (fun s -> s <> "")
    (String.split_on_char ' ' s) with
    | _ :: rest -> rest
    | _ -> []
;;

let first_problem file =
    let time_line = input_line file in
    let dist_line = input_line file in

    let times = List.map (fun x -> int_of_string x) (parse_data time_line) in
    let dists = List.map (fun x -> int_of_string x) (parse_data dist_line) in
    let rec iterate_over ts rs =
        match (ts, rs) with
        | (t :: rts, r :: rrs) -> 
            let rec iterate_rover hold =
                if hold >= t then 0
                else if (t - hold)*hold > r then
                    1 + iterate_rover (hold+1)
                else iterate_rover (hold+1)
            in
            (iterate_rover 0)*(iterate_over rts rrs)
        | _ -> 1
    in
    print_int (iterate_over times dists);
    print_char '\n'
;;

let second_problem file = 
    let time_line = input_line file in
    let dist_line = input_line file in

    let times = parse_data time_line in
    let dists = parse_data dist_line in
    
    let t = int_of_string (String.concat "" times) in
    let r = int_of_string (String.concat "" dists) in

    (* (t - ht)*ht > r
        ht*ht - t*ht + r < 0
     *)
    let d = float_of_int (t*t - 4*r) in
    let left = int_of_float (((float_of_int t) -. (sqrt d))/.2.0) in
    let right = int_of_float (((float_of_int t) +. (sqrt d))/.2.0) in
    Printf.printf "%d\n" (right-left)
;;

let () =
    let file = open_in "input.txt" in
    second_problem file
;;

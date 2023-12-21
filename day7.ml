let explode s =
    List.init (String.length s) (String.get s)
;;

let rec print_tuples ts = 
    match ts with
    | f :: r ->
        let (a, b) = f in
        Printf.printf "%s %d\n" a b;
        print_tuples r;
    | [] -> print_endline "<------------>";
;;

let rec get_rank : char list -> (char*int) list -> int = 
    fun cards counters ->
    let rec add card prefix postfix =
        match postfix with
        | f :: r ->
                let (v, count) = f in
                if v = card then
                    prefix @ [(v, count+1)] @ r
                else
                    add card (prefix @ [f]) r
        | _ -> prefix @ [(card, 1)]
                in
    match cards with
    | f :: r ->
            get_rank r (add f [] counters)
    | [] ->
        let len = List.length counters in
        if 1 = len then 7

        else let first = match counters with
        | f :: _ -> (match f with | (a, b) -> b)
        | _ -> 0 in

        if 2 = len then
            (if 1 = first || 4 = first then 6
            else if 2 = first || 3 = first then 5
            else 0)

            else let second = match counters with
            | _ :: s :: _ -> (match s with | (a, b) -> b)
            | _ -> 0 in
            let third = match counters with
            | _ :: _ :: t :: _ -> (match t with (a, b) -> b)
            | _ -> 0 in

            if 3 = len then
                (if 3 = first || 3 = second || 3 = third then 4
                 else if 2 = first || 2 = second || 2 = third then 3
                 else 0)

            else if 4 = len then 2
            else 1
;;


let compare_hands a b =
    let x = match a with | (f, s) -> f in
    let y = match b with | (f, s) -> f in

    let hand1 = explode x in
    let hand2 = explode y in

    let rank1 = get_rank hand1 [] in
    let rank2 = get_rank hand2 [] in

    (if rank1 > rank2 then 1
    else if rank1 < rank2 then -1
    else
    (let rename = String.map (fun ch ->
        match ch with
        | '2' -> 'a' 
        | '3' -> 'b'
        | '4' -> 'c'
        | '5' -> 'd'
        | '6' -> 'e'
        | '7' -> 'f'
        | '8' -> 'g'
        | '9' -> 'h'
        | 'T' -> 'i'
        | 'J' -> 'j'
        | 'Q' -> 'k'
        | 'K' -> 'l'
        | 'A' -> 'm'
        | _ -> ' '
        ) in
    let nx = rename x in
    let ny = rename y in

    if nx > ny then 1
    else if nx < ny then -1
    else 0))
;;

let first_problem file =
    let lines =
        let rec read_lines () =
            try
                let line = input_line file in
                line :: read_lines ()
            with | End_of_file -> []
        in read_lines ()
    in

    let data = List.map (fun line ->
        match String.split_on_char ' ' line with
        | hand :: bid_str :: _ -> (hand, int_of_string bid_str)
        | _ -> ("", 0)) lines
    in

    let sorted_data = List.sort compare_hands data in
    print_tuples sorted_data;

    let rec calculate l i =
        match l with
        | f :: r ->
            (match f with | (a, b) -> i*b + calculate r (i+1))
        | _ -> 0 
    in
    
    Printf.printf "res: %d" (calculate sorted_data 1);
    print_char '\n';
;;


let () = 
    let file = open_in "input.txt" in
    (* 32T3K
       KTJJT
       KK677
       T55J5
       QQQJA
    *)
    first_problem file
;;

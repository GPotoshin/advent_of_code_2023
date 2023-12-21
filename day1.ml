let compare_prefix string prefix = 
    0 == String.compare prefix (String.sub string 0 (min (String.length prefix) (String.length string)))
;;

let rec shift_and_call str pref nums=
        get_numbers_from
        (String.sub str 1 (String.length str - 1))
        (nums ^ match pref with
        |"one" -> "1"
        |"two" -> "2"
        |"three" -> "3"
        |"four" -> "4"
        |"five" -> "5"
        |"six" -> "6"
        |"seven" -> "7"
        |"eight" -> "8"
        |"nine" -> "9"
        )
and get_numbers_from line numbers = 
    match line with
    | "" -> numbers
    | _ ->
    if '0' <= line.[0] && line.[0] <= '9' then
            get_numbers_from (String.sub line 1 (String.length line - 1)) 
            (numbers ^ (String.make 1 line.[0]))
    else if compare_prefix line "one" then
        shift_and_call line "one" numbers
    else if compare_prefix line "two" then
        shift_and_call line "two" numbers
    else if compare_prefix line "three" then
        shift_and_call line "three" numbers
    else if compare_prefix line "four" then
        shift_and_call line "four" numbers
    else if compare_prefix line "five" then
        shift_and_call line "five" numbers
    else if compare_prefix line "six" then
        shift_and_call line "six" numbers
    else if compare_prefix line "seven" then
        shift_and_call line "seven" numbers
    else if compare_prefix line "eight" then
        shift_and_call line "eight" numbers
    else if compare_prefix line "nine" then
        shift_and_call line "nine" numbers
    else
        get_numbers_from
        (String.sub line 1 (String.length line - 1))
        numbers
;;

let rec get_sum file sum = 
    match input_line file with
    | exception End_of_file -> sum
    | line ->
        let numbers = get_numbers_from line "" in
        get_sum file (sum  + ((int_of_char numbers.[0]) - (int_of_char '0'))*10
        + (int_of_char numbers.[String.length numbers - 1] - int_of_char '0'))
;;

let () =
   let file = open_in "day1_input.txt" in
   print_int (get_sum file 0);
   close_in file
;;

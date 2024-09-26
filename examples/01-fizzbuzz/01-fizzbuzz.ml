let fizzbuzz fizz_num buzz_num =
  List.map (fun x -> 
    if x mod fizz_num = 0 && x mod buzz_num = 0 then print_endline "fizzbuzz"  
    else if x mod fizz_num = 0 then print_endline "fizz"
    else if x mod buzz_num = 0 then print_endline "buzz"
    else print_endline (Int.to_string(x))
    ) (List.init 100 (fun x -> x + 1));;
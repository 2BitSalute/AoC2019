let required_fuel (x : string) : int =
    Printf.printf "Module weight: %s " x;
    -2 + int_of_float (floor ((float_of_int (int_of_string x)) /. 3.))

let () =
    Printf.printf "Yay";
    let chan = (open_in "./input") in
    let rec read_input acc =
        try
            let line = input_line chan in
            let x = required_fuel line in
            Printf.printf "Fuel required: %d\n%!" x;
            read_input (acc + x)
        with
        | End_of_file ->
            (* Fun fact: trying to close the channel here results in a silent crash *)
            close_in chan;
            acc
        | _e -> Printf.printf "Some other error!"; acc
    in
    let total = read_input 0 in
    close_in chan;
    Printf.printf "Total: %d\n%!" total;
    ()
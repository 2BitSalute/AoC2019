let required_fuel (input : string) : int =
    Printf.printf "Module weight: %s -- " input;
    let rec required_fuel acc mass =
        let fuel = -2 + int_of_float (floor ((float_of_int mass) /. 3.)) in
        if fuel > 0 then required_fuel (acc + fuel) fuel
        else acc
    in
    let mass = int_of_string input in
    required_fuel 0 mass
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
            acc
        | _e -> Printf.printf "Some other error!"; acc
    in
    let total = read_input 0 in
    close_in chan;
    Printf.printf "Total fuel required: %d\n%!" total;
    ()
open Core

let () =
    let chan = (In_channel.create "./input") in
    match In_channel.input_line chan with
    | None -> failwith "no input"
    | Some input ->
        In_channel.close chan;
        let orig_ops = String.split ~on:',' input in

        for noun = 0 to 99 do
            for verb = 0 to 99 do
                let ops = Array.of_list (List.map orig_ops ~f:int_of_string) in
                let modify index f =
                    let l_pos = ops.(index + 1) in
                    let r_pos = ops.(index + 2) in
                    let dest = ops.(index + 3) in
                    ops.(dest) <- f ops.(l_pos) ops.(r_pos)
                in
                let rec exec (index : int) : unit =
                    match ops.(index) with
                    | 99 -> ()
                    | 1 ->
                        modify index (+);
                        exec (index + 4)
                    | 2 ->
                        modify index ( * );
                        exec (index + 4)
                    | _ -> failwith "error"
                in
                ops.(1) <- noun;
                ops.(2) <- verb;
                exec 0;
                if ops.(0) = 19690720 then failwith (Printf.sprintf "ANSWER - [%d - %d]" noun verb) else ()
            done
        done;
        Printf.printf "ok good"
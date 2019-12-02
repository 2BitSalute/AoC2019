open Core

let () =
    let chan = (In_channel.create "./input") in
    let input = Option.value_exn (In_channel.input_line chan) in
    let orig_ops = String.split ~on:',' input in

    for noun = 0 to 99 do
        for verb = 0 to 99 do
            let ops = Array.of_list (List.map orig_ops ~f:int_of_string) in
            ops.(1) <- noun;
            ops.(2) <- verb;
            let rec exec (index : int) : unit =
                let apply index f =
                    let l_pos = ops.(index + 1) in
                    let r_pos = ops.(index + 2) in
                    let dest = ops.(index + 3) in
                    ops.(dest) <- f ops.(l_pos) ops.(r_pos);
                    exec (index + 4)
                in
                match ops.(index) with
                | 99 -> ()
                | 1 -> apply index (+)
                | 2 -> apply index ( * )
                | _ -> failwith "error"
            in
            exec 0;
            if ops.(0) = 19690720 then failwith (Printf.sprintf "ANSWER - [%d - %d]" noun verb) else ()
        done
    done
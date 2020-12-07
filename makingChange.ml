let value = int_of_float (read_float() *. 100.);;

let find_num_coin value = 
    let change = value mod 100 in
    let () = Printf.printf "$0.%d " change in
    let coin_list = [(25, "Q");(10, "D");(5, "N");(1, "P")] in
    let rec iter (lst, target, num) = 
        match lst with
            | (a, b)::t -> 
            	if a > target then 
            		iter(t, target, num) 
            	else 
            		let () = Printf.printf "%s " b in 
            		iter(lst, target - a, num + 1)
            | _ -> num
    in
    iter(coin_list, change, 0);;

let get_num = find_num_coin value;;
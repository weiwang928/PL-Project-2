let level = 4;;
let pobits = 12;;

let page_size = (Int.shift_left 1 pobits) + 1;;
let flag_index = Int.shift_left 1 pobits;;

let ptbr = Array.make page_size (
    Some (Array.make page_size (
        Some (Array.make page_size (
            Some (Array.make page_size (None : int option array option)))))));;

let kth_vpn (va, k) = 
    let level = Int64.of_int level in
    let pobits = Int64.of_int pobits in
    let k = Int64.of_int k in
    if (Int64.compare k 0L) < 0 || (Int64.compare k level) >= 0 then
        Int64.minus_one
    else
        let unused = Int64.sub 64L (Int64.add pobits (Int64.mul level (Int64.sub pobits 3L))) in
        let va = Int64.shift_right va (Int64.to_int pobits) in
        let va = Int64.shift_left va (Int64.to_int (Int64.add unused pobits)) in
        let vpn_length = Int64.sub pobits 3L in
        let left_shift = Int64.mul k vpn_length in
        let right_shift = Int64.sub 64L vpn_length in
        let va = Int64.shift_left va (Int64.to_int left_shift) in
        let va = Int64.shift_right va (Int64.to_int right_shift) in
        let mask = Int64.sub (Int64.shift_left 1L (Int64.to_int vpn_length)) 1L in
        (Int64.logand va mask)

let get_po va = 
    let mask = Int64.sub (Int64.shift_left 1L pobits) 1L in
    (Int64.logand va mask)

let translate va = 
    if ptbr.(flag_index) == None then
        None
    else
        let index0 = Int64.to_int (kth_vpn(va, 0)) in
        let level1 = ptbr.(index0) in
        if (Option.get level1).(flag_index) == None then
            None
        else
            let index1 = Int64.to_int (kth_vpn(va, 1)) in
            let level2 = (Option.get level1).(index1) in
            if (Option.get level2).(flag_index) == None then
                None
            else
                let index2 = Int64.to_int (kth_vpn(va, 2)) in
                let level3 = (Option.get level2).(index2) in
                if (Option.get level3).(flag_index) == None then
                    None
                else
                    let index3 = Int64.to_int (kth_vpn(va, 3)) in
                    if (Option.get level3).(index3) == None then
                        None
                    else
                        let level4 = (Option.get level3).(index3) in
                        let offset = get_po va in
                        Some ((Option.get level4), (Int64.to_int offset));;
    

let page_allocate va = 
    let index0 = Int64.to_int (kth_vpn(va, 0)) in
    let index1 = Int64.to_int (kth_vpn(va, 1)) in
    let index2 = Int64.to_int (kth_vpn(va, 2)) in
    let index3 = Int64.to_int (kth_vpn(va, 3)) in
    let () = ptbr.(flag_index) <- Some (Array.make 1 None) in
    let level1 = ptbr.(index0) in
    let () = (Option.get level1).(flag_index) <- Some (Array.make 1 None) in
    let level2 = (Option.get level1).(index1) in
    let () = (Option.get level2).(flag_index) <- Some (Array.make 1 None) in
    let level3 = (Option.get level2).(index2) in
    let () = (Option.get level3).(flag_index) <- Some (Array.make 1 None) in
    let () = (Option.get level3).(index3) <- Some (Array.make (page_size - 1) (None : int option)) in
    if (Option.get level3).(index3) == None then
        false
    else
        true;;
        
let page_deallocate va = 
    let translate_result = translate va in
    if translate_result = None then
        true
    else
        let index0 = Int64.to_int (kth_vpn(va, 0)) in
        let index1 = Int64.to_int (kth_vpn(va, 1)) in
        let index2 = Int64.to_int (kth_vpn(va, 2)) in
        let index3 = Int64.to_int (kth_vpn(va, 3)) in
        let () = ptbr.(flag_index) <- None in
        let level1 = ptbr.(index0) in
        let () = (Option.get level1).(flag_index) <- None in
        let level2 = (Option.get level1).(index1) in
        let () = (Option.get level2).(flag_index) <- None in
        let level3 = (Option.get level2).(index2) in
        let () = (Option.get level3).(flag_index) <- None in
        let () = (Option.get level3).(index3) <- None in
        if (Option.get level3).(index3) == None then
            true
        else
            false;;

let allocated = page_allocate 81985529216486895L;;
let () = assert(allocated);;
let translate_result = translate 81985529216486895L;;
let () = assert(translate_result != None);;
let (arr, offset) = Option.get translate_result;;
arr.(offset) <- Some 3;;
let () = assert(arr.(offset) = (Some 3));;
let translate_result = translate 81985529216483328L;;
let () = assert(translate_result != None);;
let (arr, _) = Option.get translate_result;;
let () = assert(arr.(offset) = (Some 3));;
let translate_result = translate 0L;;
let () = assert(translate_result = None);;
let deallocated = page_deallocate 81985529216486895L;;
let () = assert(deallocated);;
let translate_result = translate 81985529216486895L;;
let () = assert(translate_result = None);;
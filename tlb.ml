let level = 4;;
let pobits = 12;;

type cache_line = {
    valid_bit : int;
    tag : int64;
    block : int64;
    last_used : int
};;

let tlb = Array.make 64 ({valid_bit = 0; tag = 0L; block = 0L; last_used = 0});;

let set_PO_zero va = 
    let va = Int64.shift_right va pobits in
    let va = Int64.shift_left va pobits in
    va;;
    
let tlb_clear = 
    let rec iter index = 
        match index with
            | 64 -> true
            | _ -> 
                let () = tlb.(index) <- ({valid_bit = 0; tag = 0L; block = 0L; last_used = 0}) in
                iter (index + 1)
    in
    iter 0;;
    
let get_VPN_portion va = 
    let mask = Int64.shift_left 1L (pobits + (level * (pobits - 3))) in
    let mask = (Int64.to_int mask) - 1 in
    let va = Int64.logand va (Int64.of_int mask) in
    let va = Int64.shift_right va pobits in
    va;;
    
let get_index va = 
    let mask = Int64.shift_left 1L 4 in
    let mask = (Int64.to_int mask) - 1 in
    let va = Int64.logand va (Int64.of_int mask) in
    va;;
    
let get_tag va = 
    let mask = Int64.shift_left 1L 60 in
    let mask = (Int64.to_int mask) - 1 in
    let va = Int64.shift_right va 4 in
    let va = Int64.logand va (Int64.of_int mask) in
    va
    
let get_POBITS va = 
    let mask = Int64.shift_left 1L pobits in
    let mask = (Int64.to_int mask) - 1 in
    let va = Int64.logand va (Int64.of_int mask) in
    va

let tlb_peek va = 
    let va = get_VPN_portion va in
    let index = Int64.to_int (get_index va) in
    let end_index = (index + 1) * 4 in
    let rec iter idx end_index = 
        if idx != end_index then
            let valid = (tlb.(idx)).valid_bit in
            let tag = (tlb.(idx)).tag in
            if (valid = 1 && (Int64.equal tag (get_tag va))) then
                ((tlb.(idx)).last_used)
            else
                iter (idx + 1) end_index
        else
            0
    in
    iter (index * 4) end_index

let translate va = va;;

let tlb_translate va = 
    let page_offset = get_POBITS va in
    let vpn = get_VPN_portion va in
    let index = Int64.to_int (get_index vpn) in
    let end_index = (index + 1) * 4 in
    let rec iter i end_i = 
        if i = end_i then
            (0L, 0, false)
        else
            let valid = (tlb.(i)).valid_bit in
            let tag = (tlb.(i)).tag in
            if ((valid = 1) && (Int64.equal tag (get_tag vpn))) then
                let block = (tlb.(i)).block in
                let last_used = (tlb.(i)).last_used in
                (block, last_used, true)
            else
                iter (i + 1) end_i
    in
    let iter_result = iter (index * 4) end_index in
    let (block, last_used, found) = iter_result in
    let change_used rank = 
        if rank != 1 then
            let rec iter i end_i lst = 
                if i != end_i then
                    let valid = (tlb.(i)).valid_bit in
                    let last_used = (tlb.(i)).last_used in
                    let tag = (tlb.(i)).tag in
                    let block = (tlb.(i)).block in
                    if ((valid = 1) && (last_used < lst)) then 
                        let () = tlb.(i) <- {valid_bit = valid; tag = tag; block = block; last_used = (last_used + 1)} in
                        iter (i + 1) end_i lst
                    else if ((valid = 1) && (last_used = lst)) then 
                        let () = tlb.(i) <- {valid_bit = valid; tag = tag; block = block; last_used = 1} in
                        iter (i + 1) end_i lst
                else
                    ()
            in
            iter (index * 4) end_index rank
        else
            ()
    in
    if not found then 
        let block = translate (set_PO_zero va) in
        let compare = Int64.equal block Int64.minus_one in
        if compare then
            -1L
        else
            let rec iter i end_i = 
                let valid = (tlb.(i)).valid_bit in
                let last_used = (tlb.(i)).last_used in
                if i != end_i then
                    if ((valid != 1) || (last_used = 4)) then
                        let tag = get_tag vpn in
                        let () = tlb.(i) <- {valid_bit = 1; tag = tag; block = block; last_used = 4} in
                        (block, 4, true)
                    else
                        iter (i + 1) end_i
                else
                    (block, 4, true)
            in
            let result = iter (index * 4) end_index in
            let (block, last_used, found) = result in
            let () = change_used 4 in
            Int64.add block page_offset
    else
        let () = change_used last_used in
        Int64.add block page_offset

let result = tlb_translate 81985529216486895L;;
let peek = tlb_peek 81985529216486895L;;
let () = assert(peek = 1);;
let result = tlb_translate 52719L;;
let peek = tlb_peek 52719L;;
let () = assert(peek = 1);;
let peek = tlb_peek 81985529216486895L;;
let () = assert(peek = 2);;
let () = assert((tlb.(48)).valid_bit = 1);;
let () = assert((tlb.(49)).valid_bit = 1);;
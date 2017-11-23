let counting_sort l =
  let rec find l min max = 
    match l with
    | [] -> (min, max)
    | _ -> let hd = List.hd l in 
            if hd < min then find (List.tl l) hd max
            else if hd > max then find (List.tl l) min hd
            else find (List.tl l) min max
  in
  let arr = Array.of_list l in
  let (lo, hi) = find l (List.hd l) 0 in
  let count = Array.make (hi-lo+1) 0 in
    Array.iter (fun i -> count.(i-lo) <- count.(i-lo) + 1) arr;
    Array.to_list (Array.concat (Array.to_list (Array.mapi (fun i x -> Array.make x (lo+i)) count)))
;;

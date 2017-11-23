let bucket_sort l =
  let rec find l min max = 
    match l with
    | [] -> (min, max)
    | _ -> let hd = List.hd l in 
            if hd < min then find (List.tl l) hd max
            else if hd > max then find (List.tl l) min hd
            else find (List.tl l) min max in
  let (min, max) = find l (List.hd l) 0 in
  let step = (max - min) / 2 in
  let res = Array.make step [] in
  List.iter (fun hd -> let m = (hd / step) in Array.set res m (hd::res.(m))) l;
  List.concat (Array.to_list (Array.map (fun a -> (quick_sort a)) res ))
;;  

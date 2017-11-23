let shuffle d = 
  Random.self_init ();
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond
;;

let rec range a b =
  if a > b then []
  else a :: range (a+1) b
;;

let test = shuffle (range 0 500);;

let insert_sort = 
  let rec insert lst x = 
    match lst with
      [] -> [x]
    | y :: ys  when x <= y -> x :: y :: ys
    | y :: ys -> y :: insert ys x
    in
  List.fold_left insert []
;;

let rec merge_sort = 
  let rec merge = function
  | list, []
  | [], list -> list
  | h1::t1, h2::t2 ->
      if h1 <= h2 then
        h1 :: merge (t1, h2::t2)
      else
        h2 :: merge (h1::t1, t2)
  in
  let rec halve = function
  | []
  | [_] as t1 -> t1, []
  | h::t ->
      let t1, t2 = halve t in
        h::t2, t1
  in
  function
  | []
  | [_] as list -> list
  | list ->
    let l1, l2 = halve list in
      merge (merge_sort l1, merge_sort l2)
;;

let rec quick_sort = function
    | [] -> []
    | x::xs -> let smaller, larger = List.partition (fun y -> y < x) xs
               in quick_sort smaller @ (x::quick_sort larger)
;;

let rec quick_sort_r = function
  | [] -> []
  | x::xs -> 
    let num = Random.int x in
    let smaller, larger = List.partition (fun y -> y < num) xs
           in quick_sort smaller @ (x::quick_sort larger)
;;

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

let radix_sort l =
  let is0 x =
    match x with
      0 -> true
    | n -> false
  in
  let splitBy n xxs =
    let rec splitBy' n' xxs' (oz,iz) =
       match xxs' with
         [] -> (oz,iz)
       |  x :: xs -> splitBy' n' xs (if is0 ((x lsr n') land 1) then (x::oz,iz) else (oz,x::iz))
      in
    splitBy' n xxs ([],[])  
      in
      let reverse ls =
        let rec reverse' (xs,scratch) =
           match xs with
             [] -> scratch
           | x :: xs -> reverse' (xs,x :: scratch)
           in
        reverse' (ls,[])
      in
    let join (oz,iz) = List.concat [reverse oz;reverse iz] in
    let sortBy n xs = join (splitBy n xs) in    
    let rec sort' n xs' =
        match n with
          0 -> xs'
        | i -> sortBy (i-1) (sort' (i-1) xs')
      in  
     sort' 32 l
;;

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

let time s f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "%s execution time: %fs\n" s (Sys.time() -. t);
  fx
;;

time "insert_sort" insert_sort test;;
time "merge_sort" merge_sort test;;
time "quick_sort" quick_sort test;;
time "quick_sort_r" quick_sort_r test;;
time "radix_sort" radix_sort test;;
time "bucket_sort" bucket_sort test;;
time "counting_sort" counting_sort test;;
let insert_sort = 
  let rec insert lst x = 
    match lst with
      [] -> [x]
    | y :: ys  when x <= y -> x :: y :: ys
    | y :: ys -> y :: insert ys x
    in
  List.fold_left insert []
;;

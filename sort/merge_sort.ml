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

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

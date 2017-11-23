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

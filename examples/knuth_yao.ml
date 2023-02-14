open Byoppl
open Distribution
open Basic
open Effect

let dice () =
  let rec gen x =
    let c = perform (Sample (bernoulli ~p:0.5)) in
    match x with
    | 0 -> gen (if c = 1 then 1 else 2)
    | 1 -> gen (if c = 1 then 3 else 4)
    | 2 -> gen (if c = 1 then 5 else 6)
    | 3 -> gen (if c = 1 then 1 else 11)
    | 4 -> gen (if c = 1 then 12 else 13)
    | 5 -> gen (if c = 1 then 14 else 15)
    | 6 -> gen (if c = 1 then 16 else 2)
    | _ -> x
  in
  gen 0

let _ =
  let dist = Infer.Importance_sampling.infer dice () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %d %f@." i x probs.(i)) values

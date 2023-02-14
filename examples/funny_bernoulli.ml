open Effect
open Byoppl
open Distribution
open Basic

let funny_bernoulli () =
  let a = perform (Sample (bernoulli ~p:0.5)) in
  let b = perform (Sample (bernoulli ~p:0.5)) in
  let c = perform (Sample (bernoulli ~p:0.5)) in
  let () = perform (Assume (a = 1 || b = 1)) in
  a + b + c

let _ =
  Format.printf "@.-- Funny Bernoulli, Basic Rejection Sampling --@.";
  let dist = Rejection_sampling.infer funny_bernoulli () in
  let { values; probs; _ } = Distribution.get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values

let _ =
  Format.printf "@.-- Funny Bernoulli, Basic Importance Sampling --@.";
  let dist = Importance_sampling.infer funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values

let _ =
  Format.printf "@.-- Funny Bernoulli, CPS Generation --@.";
  for _ = 1 to 10 do
    let v = Infer.Gen.draw funny_bernoulli () in
    Format.printf "%d " v
  done;
  Format.printf "@."

let _ =
  Format.printf "@.-- Funny Bernoulli, CPS Importance Sampling --@.";
  let dist = Infer.Importance_sampling.infer funny_bernoulli () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values

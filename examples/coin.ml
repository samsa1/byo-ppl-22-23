open Effect
open Byoppl
open Distribution
open Basic
open Basic.Rejection_sampling

let coin data =
  let z = perform (Sample (uniform ~a:0. ~b:1.)) in
  let () = List.iter (fun v -> perform (Observe (bernoulli ~p:z, v))) data in
  z

let _ =
  Format.printf "@.-- Coin, Basic Rejection Sampling --@.";
  let dist = infer coin [ 0; 1; 1; 0; 0; 0; 0; 0; 0; 0 ] in
  let m, s = Distribution.stats dist in
  Format.printf "Coin bias, mean: %f std:%f@." m s

let _ =
  Format.printf "@.-- Coin, Basic Importance Sampling --@.";
  let dist = Importance_sampling.infer coin [ 0; 1; 1; 0; 0; 0; 0; 0; 0; 0 ] in
  let m, s = Distribution.stats dist in
  Format.printf "Coin bias, mean: %f std:%f@." m s

let _ =
  Format.printf "@.-- Coin, CPS Importance Sampling --@.";
  let dist = Infer.Importance_sampling.infer coin [ 0; 1; 1; 0; 0; 0; 0; 0; 0; 0 ] in
  let m, s = Distribution.stats dist in
  Format.printf "Coin bias, mean: %f std:%f@." m s

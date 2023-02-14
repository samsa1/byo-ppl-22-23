open Byoppl
open Distribution
open Basic
open Effect

let laplace () =
  let p = perform (Sample (uniform ~a:0. ~b:1.)) in
  (* let g = sample prob (binomial ~p ~n:493_472) in
     let () = assume prob (g = 241_945) in *)
  let () = perform (Observe (binomial ~p ~n:493_472, 241_945)) in
  p

(* open Basic.Rejection_sampling

   let laplace prob () =
     let p = sample prob (uniform ~a:0. ~b:1.) in
     let () = observe prob (binomial ~p ~n:493_472) 241_945 in
     p

   let _ =
     Format.printf "@.-- Laplace, Basic Rejection Sampling --@.";
     Format.printf "Warning: never terminate...@.";
     let dist = infer laplace () in
     let m, s = Distribution.stats dist in
     Format.printf "Gender bias, mean: %f std:%f@." m s *)

let _ =
  Format.printf "@.-- Laplace, Basic Importance Sampling --@.";
  let dist = Importance_sampling.infer laplace () in
  let m, s = Distribution.stats dist in
  Format.printf "Gender bias, mean: %f std:%f@." m s

let _ =
  Format.printf "@.-- Laplace, CPS Importance Sampling --@.";
  let dist = Infer.Importance_sampling.infer laplace () in
  let m, s = Distribution.stats dist in
  Format.printf "Gender bias, mean: %f std:%f@." m s

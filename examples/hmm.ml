open Byoppl
open Distribution
open Basic
open Effect

let hmm data =
  let rec gen states data =
    match (states, data) with
    | [], y :: data -> gen [ y ] data
    | states, [] -> states
    | pre_x :: _, y :: data ->
        let x = perform (Sample (gaussian ~mu:pre_x ~sigma:1.0)) in
        let () = perform (Observe (gaussian ~mu:x ~sigma:1.0, y)) in
        gen (x :: states) data
  in
  gen [] data

let _ =
  Format.printf "@.-- HMM, Basic Importance Sampling --@.";
  let data = Owl.Arr.linspace 0. 20. 20 |> Owl.Arr.to_array |> Array.to_list in
  let dist = Distribution.split_list (Importance_sampling.infer hmm data) in
  let m_x = List.map Distribution.mean (List.rev dist) in
  List.iter2 (Format.printf "%f >> %f@.") data m_x

let _ =
  Format.printf "@.-- HMM, CPS Importance Sampling --@.";
  let data =
    Owl.Arr.(linspace 0. 20. 20 + gaussian [| 20 |])
    |> Owl.Arr.to_array |> Array.to_list
  in
  let dist = Distribution.split_list (Infer.Importance_sampling.infer hmm data) in
  let m_x = List.map Distribution.mean (List.rev dist) in
  List.iter2 (Format.printf "%f %f@.") data m_x

let _ =
  Format.printf "@.-- HMM, CPS Particle Filter --@.";
  let data =
    Owl.Arr.(linspace 0. 20. 20 + gaussian [| 20 |])
    |> Owl.Arr.to_array |> Array.to_list
  in
  let dist = Distribution.split_list (Infer.Particle_filter.infer hmm data) in
  let m_x = List.map Distribution.mean (List.rev dist) in
  List.iter2 (Format.printf "%f %f@.") data m_x

let _ =
  Format.printf "@.-- HMM, CPS MCMC --@.";
  let data =
    Owl.Arr.(linspace 0. 20. 20 + gaussian [| 20 |])
    |> Owl.Arr.to_array |> Array.to_list
  in
  let dist = Distribution.split_list (Infer.Metropolis_hasting.infer hmm data) in
  let m_x = List.map Distribution.mean (List.rev dist) in
  List.iter2 (Format.printf "%f %f@.") data m_x

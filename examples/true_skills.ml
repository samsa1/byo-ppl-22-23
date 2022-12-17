open Byoppl
open Distribution
open Cps_operators
open Infer.Importance_sampling

let true_skills () =
  let* skill_a = sample (gaussian ~mu:100. ~sigma:10.) in
  let* skill_b = sample (gaussian ~mu:100. ~sigma:10.) in
  let* skill_c = sample (gaussian ~mu:100. ~sigma:10.) in
  let* perf_a1 = sample (gaussian ~mu:skill_a ~sigma:15.) in
  let* perf_b1 = sample (gaussian ~mu:skill_b ~sigma:15.) in
  let* () = assume (perf_a1 > perf_b1) in
  let* perf_b2 = sample (gaussian ~mu:skill_b ~sigma:15.) in
  let* perf_c2 = sample (gaussian ~mu:skill_c ~sigma:15.) in
  let* () = assume (perf_b2 > perf_c2) in
  let* perf_a3 = sample (gaussian ~mu:skill_a ~sigma:15.) in
  let* perf_c3 = sample (gaussian ~mu:skill_c ~sigma:15.) in
  let* () = assume (perf_a3 > perf_c3) in
  return [ skill_a; skill_b; skill_c ]

let _ =
  let dist = split_list (infer true_skills ()) in
  List.iter2
    (fun n d -> Format.printf "%s: mean: %f, std: %f@." n (mean d) (std d))
    [ "A"; "B"; "C" ] dist

open Infer.Particle_filter

let true_skills (players, results) =
  let* skills =
    Cps_array.map (fun mu -> sample (gaussian ~mu ~sigma:10.)) players
  in
  let play (winner, looser) =
    let* perf_winner = sample (gaussian ~mu:skills.(winner) ~sigma:15.) in
    let* perf_looser = sample (gaussian ~mu:skills.(looser) ~sigma:15.) in
    assume (perf_winner > perf_looser)
  in
  let* () = Cps_list.iter play results in
  return skills

let _ =
  let dist =
    split_array
      (infer true_skills ([| 100.; 100.; 100. |], [ (0, 1); (1, 2); (0, 2) ]))
  in
  Array.iter2
    (fun n d -> Format.printf "%s: mean: %f, std: %f@." n (mean d) (std d))
    [| "A"; "B"; "C" |] dist

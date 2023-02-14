open Byoppl
open Distribution
open Basic
open Effect

let true_skills () =
  let skill_a = perform (Sample (gaussian ~mu:100. ~sigma:10.)) in
  let skill_b = perform (Sample (gaussian ~mu:100. ~sigma:10.)) in
  let skill_c = perform (Sample (gaussian ~mu:100. ~sigma:10.)) in
  let perf_a1 = perform (Sample (gaussian ~mu:skill_a ~sigma:15.)) in
  let perf_b1 = perform (Sample (gaussian ~mu:skill_b ~sigma:15.)) in
  let () = perform (Assume (perf_a1 > perf_b1)) in
  let perf_b2 = perform (Sample (gaussian ~mu:skill_b ~sigma:15.)) in
  let perf_c2 = perform (Sample (gaussian ~mu:skill_c ~sigma:15.)) in
  let () = perform (Assume (perf_b2 > perf_c2)) in
  let perf_a3 = perform (Sample (gaussian ~mu:skill_a ~sigma:15.)) in
  let perf_c3 = perform (Sample (gaussian ~mu:skill_c ~sigma:15.)) in
  let () = perform (Assume (perf_a3 > perf_c3)) in
  [ skill_a; skill_b; skill_c ]

let _ =
  let dist = split_list (Infer.Importance_sampling.infer true_skills ()) in
  List.iter2
    (fun n d -> Format.printf "%s: mean: %f, std: %f@." n (mean d) (std d))
    [ "A"; "B"; "C" ] dist


let true_skills (players, results) =
  let skills =
    Array.map (fun mu -> perform (Sample (gaussian ~mu ~sigma:10.))) players
  in
  let play (winner, looser) =
    let perf_winner = perform (Sample (gaussian ~mu:skills.(winner) ~sigma:15.)) in
    let perf_looser = perform (Sample (gaussian ~mu:skills.(looser) ~sigma:15.)) in
    perform (Assume (perf_winner > perf_looser))
  in
  Array.iter play results; skills

let _ =
  let dist =
    split_array
      (Infer.Particle_filter.infer true_skills ([| 100.; 100.; 100. |], [| (0, 1); (1, 2); (0, 2) |]))
  in
  Array.iter2
    (fun n d -> Format.printf "%s: mean: %f, std: %f@." n (mean d) (std d))
    [| "A"; "B"; "C" |] dist

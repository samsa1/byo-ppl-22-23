[@@@warning "-5"]
open Basic
open Effect
open Effect.Deep

module Gen = struct
  (* type 'a prob = 'a option
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample d k prob = 
    let v = Distribution.draw d in
    k v prob

  let factor _s k prob =
    k () prob

  let assume p = factor (if p then 0. else -. infinity)
  let observe d v = factor (Distribution.logpdf d v)

  let exit v _prob = Some v

  let draw model data = 
    let v = model data exit None in
    Option.get v *)

  let draw (model : 'a -> 'b) (data : 'a) : 'b =
    let factor _s = Some(fun k -> continue k ()) in
    try_with model data
    {
      effc = fun (type a) (eff: a t) ->
        match eff with
        | Sample d -> Some (fun k -> continue k (Distribution.draw d))
        | Factor s -> factor s
        | Assume p -> factor (if p then 0. else -. infinity)
        | Observe (d, v) -> factor (Distribution.logpdf d v)
        | _ -> None
    }
end

module Importance_sampling = struct
  type 'a prob = {id: int; particles : 'a particle Array.t }
  and 'a particle = {value: 'a option; score : float}


  let factor s k prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- {particle with score = s +. particle.score };
    continue k ()

  let rec run_next prob model data =
    if prob.id < Array.length prob.particles
    then
      let v = try_with model data 
      {
        effc = fun (type a) (eff: a t) ->
          match eff with
          | Sample d -> Some (fun k -> continue k (Distribution.draw d))
          | Factor s -> Some (fun k -> factor s k prob)
          | Assume p -> Some(fun k -> factor (if p then 0. else -. infinity) k prob)
          | Observe (d, v) -> Some(fun k -> factor (Distribution.logpdf d v) k prob)
          | _ -> None
      } in
      let particle = prob.particles.(prob.id) in
      let () = prob.particles.(prob.id) <- {particle with value = Some v} in
        run_next {prob with id = prob.id + 1} model data
    else
      prob

  let infer ?(n=1000) model data =
    let init_particles = {value = None; score = 0. } in
    let prob = {id = 0; particles = Array.make n init_particles } in
    let prob = run_next prob model data in
    let values = Array.map (fun p -> Option.get p.value) prob.particles in
    let scores = Array.map (fun p -> p.score) prob.particles in
    Distribution.support ~values ~logits:scores
  
end

module Particle_filter = struct 

  type 'a prob = {id: int; particles : 'a particle Array.t}
  and 'a particle = {value: 'a option; score : float; k: 'a next}
  and 'a next = unit -> 'a prob -> 'a prob

  let resample particles =
    let logits = Array.map (fun p -> p.score) particles in
    let values = Array.map (fun p -> {p with score = 0.}) particles in
    let dist = Distribution.support ~values ~logits in
    Array.init (Array.length particles) (fun _ -> Distribution.draw dist)

  let rec factor s k prob =
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- 
      {particle with score = s +. particle.score; k = (fun _ -> continue (Multicont.Deep.clone_continuation k) ()) };
    let prob =
      if prob.id < Array.length prob.particles - 1 then prob
      else { id = -1; particles = resample prob.particles }
    in
    run_next prob
  and run_next (prob : 'a prob) : 'a prob =
    if prob.id < Array.length prob.particles - 1
    then
      let prob = {prob with id = prob.id + 1 } in
      let k = prob.particles.(prob.id).k in
      match_with
        k ()
      {
        retc = (fun v -> v);
        exnc = raise;
        effc = fun (type a) (eff: a t) ->
          match eff with
          | Sample d -> Some (fun k -> continue k (Distribution.draw d))
          | Factor s -> Some (fun k prob -> factor s k prob)
          | Assume p -> Some(fun k prob -> factor (if p then 0. else -. infinity) k prob)
          | Observe (d, v) -> Some(fun k prob -> factor (Distribution.logpdf d v) k prob)
          | _ -> None
      } prob
    else prob

  let exit model data : unit -> 'a prob -> 'a prob = fun () ->
    let v = model data in
    fun prob ->
      let particle = prob.particles.(prob.id) in
      prob.particles.(prob.id) <- {particle with value = Some v};
      run_next prob

  let infer ?(n=1000) model data =
    let init_particles = {value = None; score = 0.; k = exit model data } in
    let prob = {id = -1; particles = Array.make n init_particles} in
    let prob = run_next prob in
    let values = Array.map (fun p -> Option.get p.value) prob.particles in
    let scores = Array.map (fun p -> p.score) prob.particles in
    Distribution.support ~values ~logits:scores
end

module Metropolis_hasting = struct
  type 'a prob = {
    mutable id : int;
    mutable trace : 'a particle list;
    mutable current_score : float;
    mutable regen_from : int;
    mutable old_trace : 'a particle list;
    mutable old_score : float;
    mutable old_val : 'a option;
    values : 'a option array;
  }

  and 'a particle =
    | Particle : {
        k : 'b -> unit;
        score : float;
        value : 'b;
        dist : 'b Distribution.t;
      }
        -> 'a particle

  let from_cont k = fun v -> continue (Multicont.Deep.clone_continuation k) v

  let sample dist k prob =
    let value = Distribution.draw dist in
    let particle = Particle { k; score = prob.current_score; value; dist } in
    prob.trace <- particle :: prob.trace;
    (* useless but in the original code *)
    prob.current_score <- prob.current_score;
    k value

  let factor s k prob =
    prob.current_score <- prob.current_score +. s;
    k ()

  let assume p = factor (if p then 0. else -.infinity)
  let observe d x = factor (Distribution.logpdf d x)

  let accept prob =
    match prob.old_val with
    | None -> 1.
    | Some _ ->
        let n_old = -.log (prob.old_trace |> List.length |> Float.of_int) in
        let n_cur = -.log (prob.trace |> List.length |> Float.of_int) in
        min 1. (exp (prob.current_score -. prob.old_score +. n_cur -. n_old))

  let exit v prob =
    if prob.id < Array.length prob.values - 1 then (
      prob.id <- prob.id + 1;
      let v =
        if Distribution.(draw(bernoulli ~p:(accept prob))) = 0 then
          (
            prob.current_score <- prob.old_score;
            prob.trace <- prob.old_trace;
            Option.get prob.old_val
          )
        else v
      in
      prob.values.(prob.id) <- Some v;
      let regen_from = Random.int (List.length prob.trace) in
      let (Particle regen) = List.nth prob.trace regen_from in
      prob.old_trace <- prob.trace;
      prob.old_val <- Some v;
      prob.old_score <- prob.current_score;
      prob.trace <- Utils.slice prob.trace regen_from;
      prob.current_score <- regen.score;
      prob.regen_from <- regen_from;
      sample regen.dist regen.k prob)

  let infer ?(n = 1000) ?(warmup = n) m data =
    let prob = {
          id = -1;
          trace = [];
          current_score = 0.;
          regen_from = 0;
          old_trace = [];
          old_val = None;
          old_score = -.infinity;
          values = Array.make (warmup + n) None;
        } in
    let () = try_with
        (fun () -> exit (m data) prob) ()
      {
        effc = fun (type a) (eff: a t) ->
          match eff with
          | Sample d -> Some (fun k -> sample d (from_cont k) prob)
          | Factor s -> Some (fun k -> factor s (from_cont k) prob)
          | Assume p -> Some(fun k -> assume p (from_cont k) prob)
          | Observe (d, v) -> Some(fun k -> observe d v (from_cont k) prob)
          | _ -> None
      } in
    let values =
      prob.values |> Array.map Option.get |> fun a ->
      Array.sub a warmup n
    in
    Distribution.uniform_support ~values
end


module Exact_sampling = struct
  type 'a prob = {
    mutable trace : 'a particle list;
    mutable current_score : float;
    mutable values : ('a, float) Hashtbl.t;
  }

  and 'a particle =
    | Particle : {
        k : 'b -> unit;
        score : float;
        values : ('b * float) list;
      }
        -> 'a particle

  exception FeatureNotSupported

  let from_cont k = fun v -> continue (Multicont.Deep.clone_continuation k) v

  let rec restart prob =
    match prob.trace with
      | [] -> ()
      | (Particle regen)::trace -> begin
        prob.trace <- trace;
        match regen.values with
          | [] -> restart prob
          | (v, s)::values -> begin
            let k = regen.k in
            prob.current_score <- regen.score +. s;
            let regen = Particle { regen with values } in
            prob.trace <- regen :: prob.trace;
            k v
          end
      end

  let sample dist k prob =
    let values = (Distribution.get_support dist).values |> Array.to_list in
    let values = List.map (fun v -> (v, Distribution.logpdf dist v)) values in
    match values with
      | [] -> restart prob
      | (v, s)::values ->
          let particle = Particle { k; score = prob.current_score; values } in
          prob.trace <- particle :: prob.trace;
          prob.current_score <- prob.current_score +. s;
          k v

  let assume p k prob =
    if p
    then continue k ()
    else begin
      restart prob
    end

  let exit v prob =
    Hashtbl.add prob.values v prob.current_score;
    restart prob

  let infer ?(n = 1000) m data =
    let prob = {
          trace = [];
          current_score = 0.;
          values = Hashtbl.create n;
        } in
    let () = try_with
        (fun () -> exit (m data) prob) ()
      {
        effc = fun (type a) (eff: a t) ->
          match eff with
          | Sample d -> Some (fun k -> sample d (from_cont k) prob)
          | Factor _ -> Some (fun k -> discontinue k FeatureNotSupported)
          | Assume p -> Some(fun k -> assume p k prob)
          | Observe _ -> Some (fun k -> discontinue k FeatureNotSupported)
          | _ -> None
      } in
    let values = prob.values |> Utils.Hashtbl.to_array_keys in
    let logits = prob.values |> Utils.Hashtbl.to_array_values in
    Distribution.support ~values ~logits
end
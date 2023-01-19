module Gen = struct
  type 'a prob = 'a option
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
    Option.get v
end

module Importance_sampling = struct
  type 'a prob = {id: int; particles : 'a particle Array.t }
  and 'a particle = {value: 'a option; score : float; k: 'a next}
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample d k prob = 
    let v = Distribution.draw d in
    k v prob

  let factor s k prob = 
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- {particle with score = s +. particle.score };
    k () prob
    
  let assume p = factor (if p then 0. else -. infinity)
  let observe d v = factor (Distribution.logpdf d v)

  let run_next prob = 
    if prob.id < Array.length prob.particles - 1 then
      let k = prob.particles.(prob.id + 1).k in
      k {prob with id = prob.id + 1}
    else
      prob

  let exit v prob = 
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- {particle with value = Some v};
    run_next prob

  let infer ?(n=1000) model data =
    let init_particles = {value = None; score = 0.; k = (model data) exit } in
    let prob = {id = -1; particles = Array.make n init_particles } in
    let prob = run_next prob in
    let values = Array.map (fun p -> Option.get p.value) prob.particles in
    let scores = Array.map (fun p -> p.score) prob.particles in
    Distribution.support ~values ~logits:scores
end

module Particle_filter = struct 
  include Importance_sampling

  let resample particles = 
    let logits = Array.map (fun p -> p.score) particles in
    let values = Array.map (fun p -> {p with score = 0.}) particles in
    let dist = Distribution.support ~values ~logits in
    Array.init (Array.length particles) (fun _ -> Distribution.draw dist)


  let factor s k prob = 
    let particle = prob.particles.(prob.id) in
    prob.particles.(prob.id) <- 
      {particle with score = s +. particle.score; k = k () };
    let prob = 
      if prob.id < Array.length prob.particles - 1 then prob
      else { id = -1; particles = resample prob.particles }
    in
    run_next prob

  let assume p = factor (if p then 0. else -. infinity)
  let observe d v = factor (Distribution.logpdf d v)

end

module Metropolis_hasting = struct
  type 'a prob = {
    id : int;
    trace : 'a particle list;
    current_score : float;
    regen_from : int;
    old_trace : 'a particle list;
    old_score : float;
    old_val : 'a option;
    values : 'a option array;
  }

  and 'a particle =
    | Particle : {
        k : 'b -> 'a next;
        score : float;
        value : 'b;
        dist : 'b Distribution.t;
      }
        -> 'a particle

  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample dist k prob =
    let value = Distribution.draw dist in
    let particle = Particle { k; score = prob.current_score; value; dist } in
    k value
      {
        prob with
        trace = particle :: prob.trace;
        current_score = prob.current_score;
      }

  let factor s k prob =
    k () { prob with current_score = prob.current_score +. s }

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
      let prob = { prob with id = prob.id + 1 } in
      let v, prob =
        if Distribution.(draw(bernoulli ~p:(accept prob))) = 0 then
          ( Option.get prob.old_val,
            { prob with current_score = prob.old_score; trace = prob.old_trace }
          )
        else (v, prob)
      in
      prob.values.(prob.id) <- Some v;
      let regen_from = Random.int (List.length prob.trace) in
      let (Particle regen) = List.nth prob.trace regen_from in
      let prob =
        {
          prob with
          trace = Utils.slice prob.trace regen_from;
          current_score = regen.score;
          regen_from;
          old_trace = prob.trace;
          old_val = Some v;
          old_score = prob.current_score;
        }
      in
      sample regen.dist regen.k prob)
    else prob

  let infer ?(n = 1000) ?(warmup = n) m data =
    let prob =
      (m data) exit
        {
          id = -1;
          trace = [];
          current_score = 0.;
          regen_from = 0;
          old_trace = [];
          old_val = None;
          old_score = -.infinity;
          values = Array.make (warmup + n) None;
        }
    in
    let values =
      prob.values |> Array.map Option.get |> fun a ->
      Array.sub a warmup n
    in
    Distribution.uniform_support ~values
end
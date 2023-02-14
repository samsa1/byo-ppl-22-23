open Effect
open Effect.Deep

type _ Effect.t += Sample : 'a Distribution.t -> 'a t
                  | Assume : bool -> unit t
                  | Factor : float -> unit t
                  | Observe : ('a Distribution.t * 'a) -> unit t

module Rejection_sampling = struct

  type prob = Prob
  
  let infer ?(n=1000) model data = 
    let rec exec i = try_with model data
      {
        effc = fun (type a) (eff: a t) ->
          match eff with
          | Sample d -> Some (fun k -> continue k (Distribution.draw d))
          | Factor _ -> Some (fun k -> continue k ())
          | Assume p -> Some(fun k -> if p then continue k () else exec i)
          | Observe (d, v) -> Some (fun k -> 
              let y = Distribution.draw d in
              if y = v
              then continue k ()
              else exec i)
          | _ -> None
      } in
    let values = Array.init n exec in
    Distribution.uniform_support ~values
end

module Importance_sampling = struct
  type prob = {id: int; scores: float Array.t}

  let factor prob s =
      prob.scores.(prob.id) <- prob.scores.(prob.id) +. s;
      Some(fun k -> continue k ())

  let infer ?(n=1000) model data = 
    let run prob model data = try_with model data
      {
        effc = fun (type a) (eff: a t) ->
          match eff with
          | Sample d -> Some (fun k -> continue k (Distribution.draw d))
          | Factor s -> factor prob s
          | Assume p -> factor prob (if p then 0. else -. infinity)
          | Observe (d, v) -> factor prob (Distribution.logpdf d v)
          | _ -> None
      } in
    let scores = Array.make n 0. in
    let values = Array.mapi (fun i _ -> run {id=i; scores} model data) scores in
    Distribution.support ~values ~logits:scores
end
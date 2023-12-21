open Byoppl
open Distribution
open Cps_operators
open Infer.Particle_filter

let foo () =
  let* c = sample (bernoulli ~p:0.5) in
  if c = 1 then
    let* () = factor 1. in
    return c
  else return c

let _ =
  let dist = infer foo () in
  let { values; probs; _ } = get_support ~shrink:true dist in
  Array.iteri (fun i x -> Format.printf "%d %f@." x probs.(i)) values

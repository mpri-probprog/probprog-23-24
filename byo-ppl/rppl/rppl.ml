open Byoppl

type 'a distribution = 'a Distribution.t

let gaussian mu sigma = Distribution.gaussian ~mu ~sigma
let bernoulli p = Distribution.bernoulli ~p
let uniform a b = Distribution.uniform ~a ~b
let draw = Distribution.draw
let mean = Distribution.mean

open Ztypes

type prob = { id : int; scores : float array } (* TODO *)

let sample (_prob, d) = draw d (* TODO *)

let factor (prob, s) =
  prob.scores.(prob.id) <- prob.scores.(prob.id) +. s (* TODO *)

let observe (prob, d, x) = factor (prob, Distribution.logpdf d x) (* TODO *)

type 'a infer_state = { mutable particles : 'a array; scores : float array }

let infer_importance n (Cnode { alloc; reset; step; copy }) =
  let infer_alloc () =
    { particles = Array.init n (fun _ -> alloc ()); scores = Array.make n 0. }
    (* TODO *)
  in
  let infer_reset state =
    Array.fill state.scores 0 n 0.;
    Array.iter reset state.particles (* TODO *)
  in
  let infer_step state data =
    let values =
      Array.mapi
        (fun i p -> step p ({ scores = state.scores; id = i }, data))
        state.particles
    in
    Distribution.support ~values ~logits:state.scores
    (* TODO *)
  in
  let infer_copy _ _ = () in
  Cnode
    {
      alloc = infer_alloc;
      reset = infer_reset;
      step = infer_step;
      copy = infer_copy;
    }

let resample alloc copy n state =
  let d = Distribution.support ~values:state.particles ~logits:state.scores in
  let particles =
    Array.init n (fun i ->
        let p = draw d in
        let p' = alloc () in
        copy p p';
        p')
  in
  state.particles <- particles;
  Array.fill state.scores 0 n 0.

let infer_pf n (Cnode { alloc; reset; step; copy }) =
  let infer_alloc () =
    { particles = Array.init n (fun _ -> alloc ()); scores = Array.make n 0. }
    (* TODO *)
  in
  let infer_reset state =
    Array.fill state.scores 0 n 0.;
    Array.iter reset state.particles (* TODO *)
  in
  let infer_step state data =
    let values =
      Array.mapi
        (fun i p -> step p ({ scores = state.scores; id = i }, data))
        state.particles
    in
    let d = Distribution.support ~values ~logits:state.scores in
    resample alloc copy n state;
    d
  in

  let infer_copy _ _ = assert false in
  Cnode
    {
      alloc = infer_alloc;
      reset = infer_reset;
      step = infer_step;
      copy = infer_copy;
    }

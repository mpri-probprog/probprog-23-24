module Gen = struct
  type 'a prob = 'a option
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample _d _k _prob = assert false
  let factor _s _k _prob = assert false
  let observe d x = factor (Distribution.logpdf d x)
  let assume p = factor (if p then 0. else -.infinity)
  let exit _v _prob = assert false
  let draw _model _data = assert false
end

module Importance_sampling = struct
  type 'a prob = { id : int; particles : 'a particle array }
  and 'a particle = { value : 'a option; score : float; k : 'a next }
  and 'a next = 'a prob -> 'a prob
  and ('a, 'b) model = 'a -> ('b -> 'b next) -> 'b next

  let sample _d _k _prob = assert false
  let factor _s _k _prob = assert false
  let assume p = factor (if p then 0. else -.infinity)
  let observe d x = factor (Distribution.logpdf d x)
  let run_next _prob = assert false
  let exit _v _prob = assert false
  let infer ?(_n = 1000) _model _data = assert false
end

module Particle_filter = struct
  let factor _s _k _prob = assert false
  let assume p = factor (if p then 0. else -.infinity)
  let observe d x = factor (Distribution.logpdf d x)
end

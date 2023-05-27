module type ROUTE = sig
  val name : string
end

module Route (T : ROUTE) = struct
  include T

  let post = T.name
end

module Increment = Route (struct
  let name = "increment"
end)

let x = Increment.post
let y = Increment.name

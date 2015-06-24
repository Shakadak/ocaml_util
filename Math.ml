
let decimal x = x -. (floor x)
let round x = if decimal x >= 0.5 then ceil x else floor x



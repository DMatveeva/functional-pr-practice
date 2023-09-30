let (.+.) x y = 
 let (zx, sx, mx) = x
 let (zy, sy, my) = y
 let sm = (mx + my) % 12
 let ss_temp = (mx + my) / 12 + sx + sy
 let ss = ss_temp % 20
 let sz = ss_temp / 20 + zx + zy
 (sz, ss, sm)

let (.-.) x y =
 let (zx, sx, mx) = x
 let (zy, sy, my) = y
 let all_mx = zx * 20 * 12 + sx * 12 + mx
 let all_my = zy * 20 * 12 + sy * 12 + my
 let d = all_mx - all_my
 let dz = d / (20 * 12)
 let ds = (d - dz * 240) / 12
 let dm = d - dz * 240 - ds * 12
 (dz, ds, dm)
    
let (.+) x y =
 let (x1: double, x2: double) = x
 let (y1: double, y2: double) = y
 (x1 + y1, x2 + y2)

let (.-) x y =
 let (x1: double, x2: double) = x
 let (y1: double, y2: double) = y
 (x1 - y1, x2 - y2)

let (.*) x y =
 let (a: double, b: double) = x
 let (c: double, d: double) = y
 (a * c - b * d, b * c + a * d)

let (./) x y =
 let (a: double, b: double) = x
 let (c: double, d: double) = y
 let n = a * c / (c * c + d * d) + b * d / (c * c + d * d) 
 let m = b * c / (c * c + d * d) - a * d / (c * c + d * d)
 (n, m)
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
 let (x1: float, x2: float) = x
 let (y1: float, y2: float) = y
 (x1 + y1, x2 + y2)

let (.-) x y =
 let (x1: float, x2: float) = x
 let (y1: float, y2: float) = y
 (x1 - y1, x2 - y2)

let (.* ) x y =
 let (a: float, b: float) = x
 let (c: float, d: float) = y
 (a * c - b * d, b * c + a * d)

let (./) x y =
 let (a: float, b: float) = x
 let (c: float, d: float) = y
 let d = a * c / (c ** 2 + d ** 2) + b * d / (c ** 2 + d ** 2) 
 let m = b * c / (c ** 2 + d ** 2) - a * d / (c ** 2 + d ** 2)
 (d, m)
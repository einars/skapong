open Printf
open Checked

(* for array_of_file *)
open Unix
open Bigarray

let ($) a b = b a

type dimension = int

let dimension_mm mm = mm * 10

let dimension_cm cm = cm * 10 $ dimension_mm

let dimension_m m = m * 1000 $ dimension_mm





let array_of_file file_name =
  let file_size = (stat file_name).st_size in
  let fd = openfile file_name [O_RDONLY] 0 in
  let contents = Array1.map_file fd int8_unsigned c_layout false file_size in
  Unix.close fd;
  contents






let tau = 6.2831852


let radians_of angle = float angle *. tau /. 360.0


let half x = x / 2
let third x = x / 3
let middle_of a b = ( a + b ) / 2


type point = int * int


type dimensions =
  { mutable width:  dimension
  ; mutable height: dimension
  }


let x_make_vector angle length =
  (float length) *. cos ( radians_of angle ) $ int_of_float,
  (float length) *. sin ( radians_of angle ) $ int_of_float


let make_vector a l =
  let x1, x2 = x_make_vector a l in
  x1, x2


let x_of pt =
  let x, _ = pt in x


let y_of pt =
  let _, y = pt in y


let (|+) pt vec =
  let x, y = pt
  and dx, dy = vec in
  x + dx, y + dy


let (|-) pt vec =
  let x, y = pt
  and dx, dy = vec in
  x - dx, y - dy


let (|%) vec percentage =
  let x, y = vec in
  x * percentage / 100, y * percentage / 100


let reflect_x vec =
  let x, y = vec in -x, y


let reflect_y vec =
  let x, y = vec in x, -y


let float_vec vec =
  let x, y = vec in float x, float y


let length_of vec =
  let fx, fy = float_vec vec in
  sqrt (fx *. fx +. fy *. fy) $ int_of_float


let string_of_pt p =
  let x, y = p in sprintf "(%d,%d)" x y


let string_of_vec v =
  let x, y = v in sprintf "[%d,%d]" x y


let lerp_pt percentage a b =
  let xa, ya = a
  and xb, yb = b in
  xa + (xb - xa) * percentage / 100, ya + (yb - ya) * percentage / 100


let lerp_int percentage a b =
  a + (b - a) * percentage / 100


let angle_of vec =
  let x, y = vec in
  if x = 0 then
    if y = 0 then 0 else if y >= 0 then 90 else 270
  else
    if y = 0 then if x > 0 then 0 else 180
    else let r = (atan2 (float y) (float x)) *. 360.0 /. tau $ int_of_float in
         if r < 0 then r + 360 else r


let vector_of seg =
  let (x1, y1),(x2, y2) = seg in
  (x2 - x1), (y2 - y1)

let clamp v lo hi =
  if v < lo then lo
  else if v > hi then hi
  else v

let is_vertical seg =
  let (x1, _), (x2, _) = seg in x1 = x2


let is_horizontal seg =
  let (_, y1), (_, y2) = seg in y1 = y2


let default_log s = try printf "%s\n%!" s with _ -> ()

let current_logger = ref default_log

let install_logger l =
  current_logger := l

let log m  = kprintf !current_logger m

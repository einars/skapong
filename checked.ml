(*

Some helpers to avoid my most frequent errors:

Reject (==) and (!=) object identity comparison (use = and <> instead),

Allow using checked addition, subtraction and multiplication, both native and 32-bit;

To install, setup:

    open Checked
    let (+) = Checked.plus
    let (-) = Checked.minus
    let ( * ) = Checked.mul

or


    open Checked
    let (+) = Checked.plus32
    let (-) = Checked.minus32
    let ( * ) = Checked.mul32

*)

module Checked = struct

  exception UndesirableFeature

  let (==) a b = raise (UndesirableFeature)
  let (!=) a b = raise (UndesirableFeature)

  exception NumericOverflow

  let plus a b =
    if b = 0 then a + b
    else
      let expect_increase = b > 0
      and res = Pervasives.(+) a b in
      if (expect_increase && res <= a) || (not expect_increase && res >= a) then begin
        Printf.printf "Overflow: %d + %d != %d\n" a b res;
        raise (NumericOverflow);
      end;
      res

  let mul a b =
    if a = 0 or b = 0 then 0
    else if b = 1 then a
    else
      let expect_increase = if a > 0 then b > 0 else b < 0
      and res = Pervasives.( * ) a b in
      if (expect_increase && res <= a) || (not expect_increase && res >= a) then begin
        Printf.printf "Overflow: %d * %d != %d\n" a b res;
        raise (NumericOverflow);
      end;
      res


  let max32 = Int64.of_int32 Int32.max_int
  let min32 = Int64.of_int32 Int32.min_int
  let plus32 a b =
    if b = 0 then a + b
    else
      let res = plus a b in
      let res64 = Int64.of_int res in
      if Int64.compare res64 max32 > 0 || Int64.compare res64 min32 < 0 then begin
        Printf.printf "32-bit overflow: %d + %d != %d\n" a b res;
        raise (NumericOverflow);
      end;
      res

  let mul32 a b =
    if a = 0 or b = 0 then 0
    else if b = 1 then a
    else
      let res = mul a b in
      let res64 = Int64.of_int res in
      if Int64.compare res64 max32 > 0 || Int64.compare res64 min32 < 0 then begin
        Printf.printf "32-bit overflow: %d * %d != %d\n" a b res;
        raise (NumericOverflow);
      end;
      res


  let minus a b = plus a (- b)
  let minus32 a b = plus32 a (- b)
end

open Common

let _threshold = 0.00001
let _equal_float f1 f2 = abs_float (f1 -. f2) < _threshold
let _equal_pos p1 p2 = _equal_float p1.x p2.x && _equal_float p1.y p2.y

module Squarify = struct

  type dir = Horizontal | Vertical

  let length dir rect = match dir with
    | Horizontal -> rect.w
    | Vertical -> rect.h
  let opp = function Horizontal -> Vertical | Vertical -> Horizontal

  type 'a state = {
    rect : rectangle ;
    dir : dir ;
    elements : 'a list ;
    area : float ;
    smallest : float ;
    biggest : float ;
  }

  let add ~area sol x =
    let a = area x in {
      sol with 
      elements = x :: sol.elements ;
      area = a +. sol.area ;
      smallest = min a sol.area ;
      biggest = max a sol.area ;
    }

  let init rect dir = {
    rect ; dir ;
    elements = [] ;
    area = 0. ;
    smallest = max_float ;
    biggest = min_float ;
  }

  (** Return the worst aspect ratio *) 
  let worst sol =
    let s = sol.area and w = length sol.dir sol.rect
    and rp = sol.biggest and rm = sol.smallest in
    max ((w*.w*.rp)/.(s*.s)) ((s*.s)/.(w*.w*.rm))

  (** Utility functions for computing layout *)
  let mv_pos dir { x ; y } len = match dir with
    | Horizontal -> { y ; x = x +. len }
    | Vertical -> { x ; y = y +. len }
  let mk_rect dir side p len = match dir with
    | Horizontal -> { p ; w = len ; h = side }
    | Vertical -> { p ; h = len ; w = side }
  let cut_rect dir { p ; w ; h } side =
    let p = mv_pos dir p side in
    match dir with
    | Horizontal -> { p ; h ; w = w -. side }
    | Vertical -> { p ; w ; h = h -. side }

  (** Layout a solution in a given rectangle. 
      Return a new state and the list laid out elements. *)
  let layout ~area ({dir ; rect ; elements ; _ } as sol) k =
    let total_len = length sol.dir rect in
    let side = sol.area /. total_len in
    let new_rect = cut_rect (opp dir) rect side in

    let layout_elem pos elem = 
      let len = total_len *. area elem /. sol.area in
      let rect = mk_rect dir side pos len in
      let pos = mv_pos dir pos len in
      k (elem, rect);
      pos
    in
    let _pos = List.fold_left layout_elem rect.p elements in
    assert (_equal_pos _pos @@ mv_pos dir rect.p total_len) ;
    init new_rect (opp dir)

  let squarify ~area rect l : _ Iter.t =
    let rec place_rect k state elem =
      let updated = add ~area state elem in
      if worst state >= worst updated then
        updated
      else
        let state = layout ~area state k in
        place_rect k state elem
    in
    let dir0 = if rect.w > rect.h then Horizontal else Vertical in
    let state0 = init rect dir0 in
    fun k ->
      let state_final = Iter.fold (place_rect k) state0 l in
      let _s =
        if state_final.elements = [] then state_final
        else layout ~area state_final k
      in
      Format.printf "Final position : %f %f@." _s.rect.w  _s.rect.h ;
      assert (_s.rect.w *. _s.rect.h >= -. _threshold);
      ()

end

let squarify = Squarify.squarify


let layout ~area ~children rect0 t0 : _ Iter.t =
  let rec go_level k (v, rect) =
    k (v, rect) ;
    let cl = children v in
    let l = squarify ~area rect cl in 
    Iter.iter (go_level k) l
  in
  let area_rect = rect0.w *. rect0.h in
  if area t0 <= area_rect +. _threshold then
    fun k -> go_level k (t0, rect0)
  else
    invalid_arg @@
    Format.sprintf
      "Tree_layout.Squarify: \
       This rectangle has area %f, \
       it can not contain a tree of area %f."
      area_rect
      (area t0)


(*
 * Copyright (c) 2019 Gabriel Radanne <drupyog@zoho.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

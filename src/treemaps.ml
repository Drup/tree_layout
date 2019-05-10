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

  let compute_dir rect =
    if rect.h > rect.w then Horizontal else Vertical

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
      rect = sol.rect ;
      dir = sol.dir ;
      elements = x :: sol.elements ;
      area = a +. sol.area ;
      smallest = min a sol.smallest ;
      biggest = max a sol.biggest ;
    }

  let init rect = {
    rect ;
    dir = compute_dir rect ;
    elements = [] ;
    area = 0. ;
    smallest = max_float ;
    biggest = 0. ;
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
      Iterate on the list of laid out elements (by continuation [k])
      and return the new state. *)
  let layout ~area sol k =
    let total_len = length sol.dir sol.rect in
    let side = sol.area /. total_len in
    let new_rect = cut_rect (opp sol.dir) sol.rect side in

    let layout_elem pos elem = 
      let len = total_len *. area elem /. sol.area in
      let rect = mk_rect sol.dir side pos len in
      let pos = mv_pos sol.dir pos len in
      k (elem, rect);
      pos
    in
    let _pos = List.fold_left layout_elem sol.rect.p (List.rev sol.elements) in
    (* assert (_equal_pos _pos @@ mv_pos sol.dir sol.rect.p total_len); *)
    new_rect

  let layout_remaining ~area sol k =
    match sol.elements with
    | [] -> ()
    | _ -> begin
        let _s = layout ~area sol k in
        assert (_s.w *. _s.h >= -. _threshold);
        ()
      end
      
  let squarify ~area rect l : _ Iter.t =
    let place_rect k state elem =
      let updated = add ~area state elem in
      if worst updated <= worst state then
        updated
      else
        let new_rect = layout ~area state k in
        let new_state = init new_rect in
        add ~area new_state elem
    in
    let state0 = init rect in
    fun k ->
      let state_final = Iter.fold (place_rect k) state0 l in
      layout_remaining ~area state_final k ;
      ()

end

let squarify = Squarify.squarify

let layout ?(sub=fun x -> x) ~area ~children rect0 l0 : _ Iter.t =
  let rec go_level k (v, rect) =
    k (v, rect) ;
    let rect = sub rect in
    let cl = children v in
    let l = squarify ~area rect cl in 
    Iter.iter (go_level k) l
  in
  fun k ->
    let l = squarify ~area rect0 l0 in
    Iter.iter (go_level k) l

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

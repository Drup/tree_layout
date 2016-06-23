(*
 * Copyright (c) 2015 Gabriel Radanne <drupyog@zoho.com>
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

type 'a sequence = ('a -> unit) -> unit

type pos = { x : float ; y : float }

(** {2 Boundaries computation} *)

type bounds = {
  mutable x0 : float ; mutable y0 : float ;
  mutable x1 : float ; mutable y1 : float ;
}

let update_bound b {x;y} =
  b.x0 <- min b.x0 x ; b.y0 <- min b.y0 y ;
  b.x1 <- max b.x1 x ; b.y1 <- max b.y1 y

let boundaries ?(margins={x=0.;y=0.;}) k =
  let b = { x0 = 0. ; y0 = 0. ; x1 = 0. ; y1 = 0. } in
  k (update_bound b) ;
  let pos = {x = b.x0 -. margins.x ; y = b.y0 -. margins.y } in
  let size = {
    x = 2.*.margins.x +. b.x1 -. b.x0 ;
    y = 2.*.margins.y +. b.y1 -. b.y0}
  in
  (pos, size)

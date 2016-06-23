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

module Opt = struct

  let get = function
    | Some x -> x
    | None -> assert false

end

(** minimal sequence stuff *)
module Seq = struct

  type 'a t = ('a -> unit) -> unit

  let fold_sibling f acc (seq : _ t) =
    let prev = ref None in
    let acc = ref acc in
    seq (fun elt -> acc := f !acc !prev elt ; prev := Some elt)

  let iter f seq = seq f

  let iteri f seq =
    let k = ref 0 in
    seq (fun elt -> f !k elt ; incr k)

end

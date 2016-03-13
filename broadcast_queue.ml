open Core_kernel.Std

(* Broadcast_queue.Make keeps track of changes that need to be broadcast to the
   cluster. *)
module Make(E: sig
    type t
    type id

    val size : t -> int
    val skip : t -> id -> bool
    val invalidates : t -> t -> bool
  end) = struct

  type t = (int * E.t) list ref

  let create () =
    ref []

  let select_broadcasts t transmit_limit addr =
    let select memo elem =
      let transmit, elems, bytes_left = memo in
      let transmit_count, broadcast = elem in
      let size = E.size broadcast in
      if transmit_count > transmit_limit then
        (transmit, elems, bytes_left)
      else if size > bytes_left || E.skip broadcast addr then
        (transmit, elem::elems, bytes_left)
      else
        (broadcast::transmit, (transmit_count+1, broadcast)::elems, bytes_left - size)
    in 
    let transmit, t', _ = List.fold_left !t ~f:select ~init:([], [], 65507) in
    t := List.sort ~cmp:(fun e e' -> Int.compare (fst e) (fst e')) t';
    transmit

  let enqueue_broadcast t broadcast =
    if not (List.exists !t ~f:(fun (_, broadcast') -> broadcast = broadcast')) then begin
      let q = List.filter !t ~f:(fun (_, broadcast') -> not (E.invalidates broadcast broadcast')) in
      t := ((0, broadcast)::q)
    end
end

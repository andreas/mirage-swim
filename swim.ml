open Lwt
open V1
open V1_LWT
open Core_kernel.Std

module Make (C : CONSOLE) (S : STACKV4) (T : TIME) (R : RANDOM) = struct
  module U = S.UDPV4
  module IPV4 = S.IPV4
  module S = Stack_ext.Make(S)

  type state = Dead | Alive with bin_io, sexp

  type addr = {
    ip   : S.ipv4addr;
    port : int;
  } with bin_io, sexp

  type node = {
    addr          : addr;
    mutable state : state;
  } with bin_io, sexp

  type payload =
    | Ping
    | Ack
    | PingReq of addr
  with bin_io, sexp

  type message = {
    seq_no  : int;
    payload : payload;
    nodes   : node list;
  } with bin_io, sexp

  (* - Ping timeout should be average or 99th percentile round-trip time
   * - Protocol period should be at least three times the round-trip estimate
   *)
  type config = {
    port                : int;
    protocol_period     : int;
    round_trip_time     : int;
    indirect_ping_count : int;
  }

  module BQ = Broadcast_queue.Make(struct
    type t = node
    type id = addr

    let invalidates node node' =
      node.addr = node'.addr && node.state <> node'.state

    let skip node addr =
      node.addr = addr

    let size = bin_size_node
  end)

  type t = {
    console           : C.t;
    net               : S.t;
    config            : config;
    transmit_queue    : BQ.t;
    acks              : (int, unit Lwt_condition.t) Hashtbl.Poly.t;
    mutable nodes     : node list;
    mutable seq_no    : int;
  }

  let create s c ~config =
    { console = c;
      net = s;
      config;
      nodes           = [];
      transmit_queue  = BQ.create ();
      acks            = Hashtbl.Poly.create ();
      seq_no          = 0;
    }

  let debug t s =
    C.log t.console s

  let debug_msg t fmt addr msg =
    let msg_s = (Sexp.to_string_hum (sexp_of_message msg)) in
    let sender_s = Sexp.to_string_hum (S.sexp_of_ipv4addr addr) in
    debug t (Printf.sprintf fmt sender_s msg_s)

  let debug_nodes t =
    debug t "Nodes:";
    debug t (t.nodes |> <:sexp_of<node list>> |> Sexp.to_string_hum)

  let next_seq_no t =
    let seq_no = t.seq_no in
    t.seq_no <- t.seq_no + 1;
    seq_no

  let node_count t =
    List.length t.nodes

  let transmit_limit t =
    if node_count t = 0 then
      0
    else
      node_count t
      |> Float.of_int
      |> log
      |> Float.round_up
      |> Float.to_int

  let send t ~addr ~payload ~seq_no =
    let transmit_limit = transmit_limit t in
    let nodes = BQ.select_broadcasts t.transmit_queue transmit_limit addr in
    let msg = { seq_no=seq_no; payload; nodes; } in
    debug_msg t "< Msg to %s: %s" addr.ip msg;
    let size = bin_size_message msg in
    let buf = Cstruct.create size in
    ignore(bin_write_message ~pos:0 (Cstruct.to_bigarray buf) msg);
    let udp = S.udpv4 t.net in
    U.write ~source_port:t.config.port ~dest_ip:addr.ip ~dest_port:addr.port udp buf

  let send_ping t ~addr ~seq_no =
    send t ~addr ~payload:Ping ~seq_no

  let send_ack t ~addr ~seq_no =
    send t ~addr ~payload:Ack ~seq_no

  let send_ping_request t ~addr ~target ~seq_no =
    send t ~addr ~payload:(PingReq target) ~seq_no

  let rec wait_ack t seq_no =
    let cond = Hashtbl.find_or_add t.acks seq_no ~default:Lwt_condition.create in
    Lwt_condition.wait cond

  let wait_ack_timeout t seq_no timeout =
    pick [
      (T.sleep (Float.of_int timeout) >> return `Timeout);
      (wait_ack t seq_no              >> return `Ok)
    ]

  let get_node_by_addr t addr =
    List.find ~f:(fun n -> n.addr = addr) t.nodes

  let update_node t node =
    match (get_node_by_addr t node.addr, node.state) with
    | (Some node, Dead) ->
        node.state <- Dead;
        BQ.enqueue_broadcast t.transmit_queue node
    | (None, Alive) ->
        t.nodes <- node::t.nodes;
        BQ.enqueue_broadcast t.transmit_queue node
    | (Some _, Alive)
    | (None, Dead) -> ()

  let sample_nodes t k exclude =
    t.nodes
    |> List.filter ~f:(fun n -> n <> exclude)
    |> List.permute
    |> fun l -> List.take l k

  let probe_node t node =
    let seq_no = next_seq_no t in
    ignore_result (send_ping t ~addr:node.addr ~seq_no);
    match_lwt (wait_ack_timeout t seq_no t.config.round_trip_time) with
    | `Ok      -> return ()
    | `Timeout ->
      debug t "ACK timeout";
      let helpers = sample_nodes t t.config.indirect_ping_count node in
      List.iter helpers (fun helper -> 
        send_ping_request t ~addr:helper.addr ~target:node.addr ~seq_no
        |> ignore_result
      );
      let wait_time = t.config.protocol_period - t.config.round_trip_time in
      match_lwt wait_ack_timeout t seq_no wait_time with
      | `Ok      -> return ()
      | `Timeout ->
          debug t "Indirect ping timeout";
          let node = { state = Dead; addr = node.addr } in 
          update_node t node;
          BQ.enqueue_broadcast t.transmit_queue node;
          return ()

  let rec failure_detection t =
    debug_nodes t;
    t.nodes <- List.filter t.nodes ~f:(fun node -> node.state = Alive);
    t.nodes <- List.permute t.nodes;
    (if node_count t = 0 then
      T.sleep (Float.of_int (t.config.protocol_period))
    else
      Lwt_list.iter_s (fun node ->
        if node.state = Dead then
          return ()
        else
          join [
            probe_node t node;
            T.sleep (Float.of_int (t.config.protocol_period))
          ]
      ) t.nodes
    ) >> 
    failure_detection t

  let join t addr =
    let seq_no = next_seq_no t in
    send_ping t ~addr ~seq_no

  let handle_payload t src_addr msg =
    update_node t {state=Alive; addr=src_addr};
    match msg.payload with
    | Ping ->
        send_ack t ~addr:src_addr ~seq_no:msg.seq_no
    | PingReq addr ->
        let seq_no = next_seq_no t in
        send_ping t ~addr ~seq_no >>
        (match_lwt wait_ack_timeout t seq_no t.config.protocol_period with
        | `Timeout -> return ()
        | `Ok      -> send_ack t ~addr:src_addr ~seq_no:msg.seq_no)
    | Ack ->
        match Hashtbl.find t.acks msg.seq_no with
        | Some cond ->
          Lwt_condition.broadcast cond ();
          return ()
        | None ->
          (* Unexpected ACK -- ignore *)
          return ()

  let callback t ~src ~dst ~src_port buf =
    let message = bin_read_message (Cstruct.to_bigarray buf) (ref 0) in
    debug_msg t "> Msg from %s: %s" src message;
    List.iter message.nodes (update_node t);
    let addr = { ip=src; port=src_port } in
    handle_payload t addr message

  let listen t =
    async (fun () -> failure_detection t);
    S.listen_udpv4 t.net ~port:t.config.port (callback t)
end

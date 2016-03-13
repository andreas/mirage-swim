open Lwt
open V1
open V1_LWT
open Printf
open Core_kernel.Std

module Main (C:CONSOLE) (S:STACKV4) (T:TIME) (R:RANDOM) = struct

  module SwimImpl = Swim.Make(C)(S)(T)(R)

  let start c s t r =
    let join_ip = Key_gen.join_ip () in
    let port       = Key_gen.port () in
    let swim = SwimImpl.create s c { port = port; protocol_period = 5; round_trip_time = 1; indirect_ping_count = 3 } in
    SwimImpl.listen swim;
    match join_ip  with
    | "" -> 
      C.log c "Starting new cluster";
      S.listen s
    | ip ->
      C.log c (Printf.sprintf "Attempting to join node: %s" ip);
      SwimImpl.join swim { ip = (Ipaddr.V4.of_string_exn ip); port = port } >>
      S.listen s
end

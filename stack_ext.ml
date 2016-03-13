open V1
open Core_kernel.Std

(* Make Ipaddr.V4.t compatible with bin_io and sexp *)
module Make(S : STACKV4) = struct
  include S

  module Ipv4Binable = Bin_prot.Utils.Make_binable(struct
    module Binable = String
    type t = Ipaddr.V4.t
    
    let to_binable = Ipaddr.V4.to_bytes
    let of_binable = Ipaddr.V4.of_bytes_exn
  end)

  let bin_size_ipv4addr  = Ipv4Binable.bin_size_t
  let bin_write_ipv4addr = Ipv4Binable.bin_write_t
  let bin_read_ipv4addr = Ipv4Binable.bin_read_t
  
  let ipv4addr_of_sexp t =
    t
    |> Sexp.to_string
    |> Ipaddr.V4.of_string_exn

  let sexp_of_ipv4addr t =
    t
    |> Ipaddr.V4.to_string
    |> Sexp.of_string
end

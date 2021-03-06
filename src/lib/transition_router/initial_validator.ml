open Core_kernel
open Async_kernel
open Pipe_lib.Strict_pipe
open Coda_base
open Coda_state
open Signature_lib
open Coda_transition
open Network_peer

type validation_error =
  [ `Invalid_time_received of [`Too_early | `Too_late of int64]
  | `Invalid_genesis_protocol_state
  | `Invalid_proof
  | `Invalid_delta_transition_chain_proof
  | `Verifier_error of Error.t
  | `Mismatched_fork_id
  | `Invalid_fork_id ]

let handle_validation_error ~logger ~trust_system ~sender ~state_hash
    (error : validation_error) =
  let open Trust_system.Actions in
  let punish action message =
    let message' =
      "external transition with state hash $state_hash"
      ^ Option.value_map message ~default:"" ~f:(fun (txt, _) ->
            sprintf ", %s" txt )
    in
    let metadata =
      ("state_hash", State_hash.to_yojson state_hash)
      :: Option.value_map message ~default:[] ~f:Tuple2.get2
    in
    Trust_system.record_envelope_sender trust_system logger sender
      (action, Some (message', metadata))
  in
  match error with
  | `Verifier_error err ->
      let error_metadata = [("error", `String (Error.to_string_hum err))] in
      Logger.fatal logger ~module_:__MODULE__ ~location:__LOC__
        ~metadata:
          (error_metadata @ [("state_hash", State_hash.to_yojson state_hash)])
        "Error in verifier verifying blockchain proof for $state_hash: $error" ;
      exit 21
  | `Invalid_proof ->
      punish Sent_invalid_proof None
  | `Invalid_delta_transition_chain_proof ->
      punish Sent_invalid_transition_chain_merkle_proof None
  | `Invalid_time_received `Too_early ->
      punish Gossiped_future_transition None
  | `Invalid_genesis_protocol_state ->
      punish Has_invalid_genesis_protocol_state None
  | `Invalid_time_received (`Too_late slot_diff) ->
      punish (Gossiped_old_transition slot_diff)
        (Some
           ( "off by $slot_diff slots"
           , [("slot_diff", `String (Int64.to_string slot_diff))] ))
  | `Invalid_fork_id ->
      punish Sent_invalid_fork_id None
  | `Mismatched_fork_id ->
      punish Sent_mismatched_fork_id None

module Duplicate_block_detector = struct
  (* maintain a map from block producer key, epoch, slot to state hashes *)

  module Blocks = struct
    module T = struct
      (* order of fields significant, compare by epoch, then slot, then producer *)
      type t =
        { consensus_time: Consensus.Data.Consensus_time.t
        ; block_producer: Public_key.Compressed.t }
      [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make (T)
  end

  type t = {mutable table: State_hash.t Blocks.Map.t; mutable latest_epoch: int}

  let delay =
    let open Consensus in
    Data.Consensus_state.network_delay Configuration.t

  let gc_width = delay * 2

  (* epoch, slot components of gc_width *)
  let gc_width_epoch = gc_width / Consensus.epoch_size

  let gc_width_slot = gc_width mod Consensus.epoch_size

  let gc_interval = gc_width

  let gc_count = ref 0

  (* create dummy block to split map on *)
  let make_splitting_block ({consensus_time; block_producer= _} : Blocks.t) :
      Blocks.t =
    let block_producer = Public_key.Compressed.empty in
    { consensus_time= Consensus.Data.Consensus_time.get_old consensus_time
    ; block_producer }

  (* every gc_interval blocks seen, discard blocks more than gc_width ago *)
  let table_gc t block =
    gc_count := (!gc_count + 1) mod gc_interval ;
    if Int.equal !gc_count 0 then
      let splitting_block = make_splitting_block block in
      let _, _, gt_map = Map.split t.table splitting_block in
      t.table <- gt_map

  let create () = {table= Map.empty (module Blocks); latest_epoch= 0}

  let check t logger external_transition_with_hash =
    let external_transition = external_transition_with_hash.With_hash.data in
    let protocol_state_hash = external_transition_with_hash.hash in
    let open Consensus.Data.Consensus_state in
    let consensus_state =
      External_transition.consensus_state external_transition
    in
    let consensus_time = consensus_time consensus_state in
    let block_producer =
      External_transition.block_producer external_transition
    in
    let block = Blocks.{consensus_time; block_producer} in
    (* try table GC *)
    table_gc t block ;
    match Map.find t.table block with
    | None ->
        t.table <- Map.add_exn t.table ~key:block ~data:protocol_state_hash
    | Some hash ->
        if not (State_hash.equal hash protocol_state_hash) then
          Logger.error logger ~module_:__MODULE__ ~location:__LOC__
            ~metadata:
              [ ( "block_producer"
                , Public_key.Compressed.to_yojson block_producer )
              ; ( "consensus_time"
                , Consensus.Data.Consensus_time.to_yojson consensus_time )
              ; ("hash", State_hash.to_yojson hash)
              ; ( "current_protocol_state_hash"
                , State_hash.to_yojson protocol_state_hash ) ]
            "Duplicate producer and slot: producer = $block_producer, \
             consensus_time = $consensus_time, previous protocol state hash = \
             $hash, current protocol state hash = $current_protocol_state_hash"
end

let run ~logger ~trust_system ~verifier ~transition_reader
    ~valid_transition_writer ~initialization_finish_signal ~genesis_state_hash
    =
  let open Deferred.Let_syntax in
  let duplicate_checker = Duplicate_block_detector.create () in
  don't_wait_for
    (Reader.iter transition_reader ~f:(fun network_transition ->
         if Ivar.is_full initialization_finish_signal then (
           let ( `Transition transition_env
               , `Time_received time_received
               , `Valid_cb is_valid_cb ) =
             network_transition
           in
           let transition_with_hash =
             Envelope.Incoming.data transition_env
             |> With_hash.of_data
                  ~hash_data:
                    (Fn.compose Protocol_state.hash
                       External_transition.protocol_state)
           in
           Duplicate_block_detector.check duplicate_checker logger
             transition_with_hash ;
           let sender = Envelope.Incoming.sender transition_env in
           let defer f = Fn.compose Deferred.return f in
           match%bind
             let open Deferred.Result.Monad_infix in
             External_transition.(
               Validation.wrap transition_with_hash
               |> defer (validate_time_received ~time_received)
               >>= defer (validate_genesis_protocol_state ~genesis_state_hash)
               >>= validate_proof ~verifier
               >>= defer validate_delta_transition_chain
               >>= defer validate_fork_ids)
           with
           | Ok verified_transition ->
               External_transition.poke_validation_callback
                 (Envelope.Incoming.data transition_env)
                 is_valid_cb ;
               Envelope.Incoming.wrap ~data:verified_transition ~sender
               |> Writer.write valid_transition_writer ;
               let blockchain_length =
                 External_transition.Initial_validated.consensus_state
                   verified_transition
                 |> Consensus.Data.Consensus_state.blockchain_length
                 |> Coda_numbers.Length.to_int
               in
               Coda_metrics.Transition_frontier.update_max_blocklength_observed
                 blockchain_length ;
               return ()
           | Error error ->
               is_valid_cb false ;
               handle_validation_error ~logger ~trust_system ~sender
                 ~state_hash:(With_hash.hash transition_with_hash)
                 error )
         else Deferred.unit ))

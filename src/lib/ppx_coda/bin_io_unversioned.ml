(* bin_io_unversioned *)

(* use [@@deriving bin_io_unversioned] for serialized types that
   are not send to nodes or persisted, namely the daemon RPC
   types for communication between the client and daemon, which
   are known to be running the same version of the software.

   The deriver here calls the derivers for bin_io after the lint
   phase. We avoid linter errors that would occur if
   we used [@@deriving bin_io] directly.
*)

open Core_kernel
open Ppxlib
open Versioned_util

let deriver = "bin_io_unversioned"

type foo = {x: int; f: unit -> unit -> int}

let _ =
  let foo = {x= 42; f= (fun () () -> eprintf "FOO\n%!" ; 99)} in
  let f : unit -> unit -> int = Obj.(obj (field (repr foo) 1)) in
  eprintf "SIZE F: %d\n%!" Obj.(size (field (repr foo) 1)) ;
  eprintf "RESULT: %d\n%!" (f () ()) ;
  eprintf "DONE\n%!"

module Foo = Ppx_deriving

let ctxt =
  let derived_item_loc = Location.none in
  let omp_config =
    Migrate_parsetree.Driver.make_config ~tool_name:"bin_io_gens" ()
  in
  let base = Expansion_context.Base.top_level ~omp_config ~file_path:"" in
  Expansion_context.Deriver.make ~derived_item_loc ~base ()

let bin_io_gens :
    (   ctxt:Expansion_context.Deriver.t
     -> rec_flag * type_declaration list
     -> structure_item list)
    list =
  List.map ["bin_read"; "bin_write"; "bin_type_class"; "bin_shape"]
    ~f:(fun d ->
      let deriver = Option.value_exn (Ppx_derivers.lookup d) in
      let actual_deriver = Obj.(obj (field (field (repr deriver) 1) 0)) in
      let name = Obj.(obj (field (repr actual_deriver) 0)) in
      eprintf "NAME: %s\n%!" name ;
      eprintf "SIZE: %d\n%!" Obj.(size (repr actual_deriver)) ;
      let generator :
          (structure, rec_flag * type_declaration list) Deriving.Generator.t =
        Option.value_exn Obj.(obj (field (repr actual_deriver) 1))
      in
      eprintf "BLOCK GENERATOR?: %B\n%!" Obj.(is_block (repr generator)) ;
      eprintf "SIZE 2: %d\n%!" Obj.(size (repr generator)) ;
      let args : Set.M(String).t = Obj.(obj (field (repr generator) 2)) in
      eprintf "NUM ARGS: %d\n%!" (Set.length args) ;
      let attrs : _ list = Obj.(obj (field (repr generator) 3)) in
      eprintf "NUM ATTRS: %d\n%!" (List.length attrs) ;
      (*      eprintf "SIZE ATTRS: %d\n%!" Obj.(size (repr attrs)) ; *)
      let deps : _ list = Obj.(obj (field (repr generator) 4)) in
      eprintf "NUM DEPS: %d\n%!" (List.length deps) ;
      let gen :
             ctxt:Expansion_context.Deriver.t
          -> rec_flag * type_declaration list
          -> structure_item list =
        Obj.(obj (field (repr generator) 1))
      in
      eprintf "SIZE 3: %d\n%!" Obj.(size (repr gen)) ;
      let _ = gen ~ctxt (Nonrecursive, []) in
      eprintf "DID GEN\n%!" ; gen )

(*
  match Ppx_derivers.lookup "bin_io" with
  | None ->
      failwith "bin_io_unversioned: could not find deriver for bin_io"
  | Some deriver ->
    let actual_deriver : Ppx_derivers.deriver = deriver in
    (* the T *)
    eprintf "DERIVER TAG: %d\n%!" Obj.(tag (repr actual_deriver));
    let inner_tag : int = Obj.(tag (field (repr actual_deriver) 1)) in
    eprintf "INNER TAG: %d\n%!" inner_tag;
    let alias = Obj.(obj (field (field (repr actual_deriver) 1) 0)) in
    eprintf "SIZE ALIAS: %d\n%!" Obj.(size (repr alias));
    let str_type_decls : string list = Obj.(obj (field alias 0)) in
    eprintf "LEN: %d\n%!" (List.length str_type_decls);
    List.iter str_type_decls ~f:(eprintf "%s ");
    eprintf "\n%!";
    (*    let deriving : Ppx_deriving.deriver = Obj.(obj (field (repr t_deriving) 1)) in
      eprintf "DERIVING: %s\n%!" Obj.(obj (field (repr deriving) 0)) ; *)
    List.iter str_type_decls ~f:(fun s ->
        match Ppx_derivers.lookup s with
          None ->
          eprintf "NOT FOUND FOR %s\n%!" s
        | Some _ ->
          eprintf "FOUND FOR %s\n%!" s);
    (fun ~options:_ ~path:_ _ty_decls -> [])

*)
let validate_type_decl inner2_modules type_decl =
  match inner2_modules with
  | [module_version; "Stable"] ->
      let inside_stable_versioned =
        try
          validate_module_version module_version type_decl.ptype_loc ;
          true
        with _ -> false
      in
      if inside_stable_versioned then
        Ppx_deriving.raise_errorf ~loc:type_decl.ptype_loc
          "Cannot use \"deriving bin_io_unversioned\" for a type defined in a \
           stable-versioned module"
  | _ ->
      ()

let rewrite_to_bin_io ~options ~path type_decls =
  let type_decl1 = List.hd_exn type_decls in
  let type_decl2 = List.last_exn type_decls in
  let loc =
    { loc_start= type_decl1.ptype_loc.loc_start
    ; loc_end= type_decl2.ptype_loc.loc_end
    ; loc_ghost= false }
  in
  if not (Int.equal (List.length type_decls) 1) then
    Ppx_deriving.raise_errorf ~loc
      "deriving bin_io_unversioned can only be used on a single type" ;
  if not @@ List.is_empty options then
    Ppx_deriving.raise_errorf ~loc
      "bin_io_unversioned does not take any options" ;
  let inner2_modules = List.take (List.rev path) 2 in
  validate_type_decl inner2_modules type_decl1 ;
  List.iter bin_io_gens ~f:(fun gen -> ignore (gen ~ctxt (Nonrecursive, []))) ;
  []

let () =
  Ppx_deriving.(register (create deriver ~type_decl_str:rewrite_to_bin_io ()))

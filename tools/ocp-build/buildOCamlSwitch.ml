(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

(* TODO:
  We should obey:
  * OCAMLFIND_TOOLCHAIN
  * OCAMLFIND_METADIR
  * OCAMLFIND_COMMANDS
  * OCAMLFIND_DESTDIR
*)

open StringCompat

type switch_config_options = {
  switch_option_name : string;
  mutable switch_option_ident : string;

  mutable switch_option_ocamlbin : string option;
  mutable switch_option_toolchain : string option;
  mutable switch_option_ocamldir : string option;
  mutable switch_option_metadir : string option;
  mutable switch_option_ocpnsdir : string option;
  mutable switch_option_ocplibdir : string option;
  mutable switch_option_bindir : string option;
}

type ocamlfind_config = {
  ocamlfind_path : string list;
}

type switch_config = {
    switch_ocamlbin : string;
    switch_ocamlrun : string;
    switch_ocamlc : string list;
    switch_ocamlcc : string list;
    switch_ocamldep : string list;

    switch_ocamldoc : string list option;
    switch_ocamlopt : string list option;
    switch_ocamlyacc : string list option;
    switch_ocamlmklib : string list option;
    switch_ocamllex : string list option;

    switch_ocamllib : string;
    switch_version : string;
    switch_version_major : string;
    switch_version_minor : string;
    switch_version_point : string;
    switch_system : string;
    switch_architecture : string;
    switch_ext_obj : string;
    switch_ext_lib : string;
    switch_ext_dll : string;
    switch_os_type : string;

    switch_ocamlfind : ocamlfind_config option;

    switch_bindir : string;
    switch_ocamldir : string;
    switch_metadir : string;
  }


module ARGUMENTS : sig

  val init : unit -> unit
  val load : unit -> (string * Arg.spec * string) list
  val save : unit -> unit

end = struct

  let switches = ref StringMap.empty

  let find_switch switch_option_name switch_option_ident =
    try
      StringMap.find switch_option_name !switches
    with Not_found ->
      let switch = {
        switch_option_name;
        switch_option_ident;
        switch_option_ocamlbin = None;
        switch_option_toolchain = None;
        switch_option_ocamldir = None;
        switch_option_metadir = None;
        switch_option_ocpnsdir = None;
        switch_option_ocplibdir = None;
        switch_option_bindir = None;
      }
      in
      switches := StringMap.add switch_option_name switch !switches;
      switch

  let load_switches () =
    assert false (* TODO: scan _obuild looking for switch configurations *)

  let current_switch = ref (find_switch "default" "")

  let select_switch switch_option_name switch_option_ident =
    let ( _ : switch_config_options) =
      find_switch switch_option_name switch_option_ident in
    ()

  let remove_switch switch_name =
    let switch = try StringMap.find switch_name !switches
      with Not_found ->
        Printf.eprintf "Error: cannot remove inexistant switch %S\n%!"
          switch_name;
        exit 2
    in
    switches := StringMap.remove switch_name !switches;
    assert false (* TODO: remove switch configuration from _obuild too *)

  let switch_args = [

    "-ocamlbin", Arg.String (fun s ->
        (!current_switch).switch_option_ocamlbin <- Some s),
    "DIR Set ocamlbin for this switch";

    "-toolchain", Arg.String (fun s ->
        (!current_switch).switch_option_toolchain <- Some s),
    "TOOLCHAIN Set toolchain for this switch";


    "-ocamldir", Arg.String (fun s ->
        (!current_switch).switch_option_ocamldir <- Some s),
    "DIR Set ocamldir for this switch";

    (* formerly, -install-meta *)
    "-metadir", Arg.String (fun s ->
        (!current_switch).switch_option_metadir <- Some s),
    "DIR Set metadir for this switch";

    "-ocpnsdir", Arg.String (fun s ->
        (!current_switch).switch_option_ocpnsdir <- Some s),
    "DIR Set ocpnsdir for this switch";

    (* formerly, -install-lib *)
    "-ocplibdir", Arg.String (fun s ->
        (!current_switch).switch_option_ocplibdir <- Some s),
    "DIR Set ocplibdir for this switch";

    (* formerly, -install-bin *)
    "-bindir", Arg.String (fun s ->
        (!current_switch).switch_option_bindir <- Some s),
    "DIR Set bindir for this switch";

  ]

  let configure_args = [
    "-switch", Arg.String (fun s -> select_switch s s),
    "SWITCH Create/configure switch SWITCH";

    "-remove", Arg.Tuple [Arg.Rest remove_switch;
                          Arg.Unit (fun () -> exit 0)],
    "SWITCHES Remove these switches and exit";

  ] @ switch_args

  let save () =
    assert false; (* TODO: save the switch configs *)
    ()

  let init () =
    assert false; (* TODO: create the default switch *)
    save ();
    ()

  let load () =
    load_switches ();
    configure_args

  end


module type INTERFACE = sig

  (* dirname of OCaml command *)
  val find_ocamlbin_in_path : string -> string list -> string option
  val find_ocamlbin : string -> string option

  (* [detect_ocaml ocamlbin] detects the switch config corresponding
     to binaries in the ocamlbin directory. Check that all the
     binaries have the same version, and prefer native-code binaries
     to bytecode binaries. Check if ocamlrun should be specified
     to execute bytecode programs. *)
  val detect_ocaml : switch_config_options -> switch_config option

  val search_path : switch_config -> string list

end

module IMPLEMENTATION : INTERFACE = struct

  let find_ocamlbin_in_path ocamlc path =
    match BuildConfig.find_first_in_path path (fun _ -> true)
            [ ocamlc; ocamlc ^ ".exe" ] with
    | None -> None
    | Some ocamlc -> Some (Filename.dirname ocamlc)

  let find_ocamlbin ocamlc =
    find_ocamlbin_in_path ocamlc (BuildConfig.get_PATH ())


  type ocaml_config = {
    ocaml_version : string;
    ocaml_version_major : string;
    ocaml_version_minor : string;
    ocaml_version_point : string;
    ocaml_ocamllib : string;
    ocaml_system : string;
    ocaml_architecture : string;
    ocaml_ext_obj : string;
    ocaml_ext_lib : string;
    ocaml_ext_dll : string;
    ocaml_os_type : string;
  }

  let get_config cmd =
    match cmd with
    | [] -> assert false
    | cmd :: args ->
      match BuildMisc.get_stdout_lines [ cmd ] (args @ [ "-config" ]) with
      | `EXN e ->
        Printf.eprintf "Warning: could not execute %S \"-config\"\n" cmd;
        None
      | `OUTPUT (_,c) ->
        let ocaml_version = ref "NOVERSION" in
        let ocaml_system = ref "NOSYSTEM" in
        let ocaml_architecture = ref "NOARCH" in
        let ocaml_ext_obj = ref ".o" in
        let ocaml_ext_lib = ref ".a" in
        let ocaml_ext_dll = ref ".so" in
        let ocaml_os_type = ref "NOOSTYPE" in
        let ocaml_ocamllib = ref "" in
        List.iter (fun line ->
            let (name, v) = OcpString.cut_at line ':' in
            let v = String.sub v 1 (String.length v - 1) in
            match name with
            | "version" -> ocaml_version := v
            | "system" -> ocaml_system := v
            | "architecture" -> ocaml_architecture := v
            | "ext_obj" -> ocaml_ext_obj := v
            | "ext_lib" -> ocaml_ext_lib := v
            | "ext_dll" -> ocaml_ext_dll := v
            | "os_type" -> ocaml_os_type := v
            | "standard_library" -> ocaml_ocamllib := v
            | _ -> ()
          ) c;
        let (major, minor, point) = BuildConfig.split_version !ocaml_version in

        Some { ocaml_version = !ocaml_version;
               ocaml_version_major = major;
               ocaml_version_minor = minor;
               ocaml_version_point = point;
               ocaml_architecture = !ocaml_architecture;
               ocaml_system = !ocaml_system;
               ocaml_ext_obj = !ocaml_ext_obj;
               ocaml_ext_lib = !ocaml_ext_lib;
               ocaml_ext_dll = !ocaml_ext_dll;
               ocaml_os_type = !ocaml_os_type;
               ocaml_ocamllib = !ocaml_ocamllib;
             }

  (* Call the command and returns the output, as a list of lines. An
     empty list is returned if an error occured, so the command should
     always print something ! *)
  let get_command_output cmd args =
    match BuildMisc.get_stdout_lines [ cmd ] args with
    | `EXN e ->
      Printf.eprintf "Warning: could not execute %S\n%!"
        (String.concat "\"" (cmd :: args));
      Printf.eprintf "\texception %S\n%!" (Printexc.to_string e);
      []
    | `OUTPUT (status, lines) ->
      if status <> 0 then begin
        Printf.eprintf "Warning: %S exited with non-zero status (%d)\n%!"
          (String.concat "\"" (cmd :: args))
          status;
        []
      end else lines

  let check_is_ocaml prefix_sep ocamlc_prefixes args ocamlc =
    match BuildMisc.get_stdout_lines [ ocamlc ] args with
    | `EXN e ->
      Printf.eprintf "Warning: could not execute %S\n%!" ocamlc;
      Printf.eprintf "\texception %S\n%!" (Printexc.to_string e);
      false
    | `OUTPUT (status, lines) ->
      if status <> 0 then begin
        Printf.eprintf "Warning: %S exited with non-zero status (%d)\n%!" ocamlc
          status;
        false
      end else
        try
          match lines with
            first_line :: _ ->
            let prefix =
              let pos = String.index first_line prefix_sep in
              String.sub first_line 0 pos
            in
            List.mem prefix ocamlc_prefixes
          | _ -> false
        with _ -> false


  let ocamlrun_cmd = (
    ["ocamlrun"] , [],
    [
      ["-version"],
      fun v ->
        [ Printf.sprintf "The Objective Caml runtime, version %s" v;
          Printf.sprintf "The OCaml runtime, version %s" v] ] )

  let ocamlc_cmd = (
    ["ocamlc.opt"], ["ocamlc"],
    [ [ "-v" ],
      fun v ->
        [ Printf.sprintf "The Objective Caml compiler, version %s" v;
          Printf.sprintf "The OCaml compiler, version %s" v] ] )

  let ocamldoc_cmd = (
    [], ["ocamldoc"],
    [ ["-version"],
      fun v -> [
          v; (* since 4.00.0 *)
          Printf.sprintf "OCamldoc %s" v (* 3.12.1 *)]
    ])

  let ocamlopt_cmd = (
    [ "ocamlopt.opt"], [ "ocamlopt" ],
    [ [ "-v" ],
      fun v ->
        [ Printf.sprintf
            "The Objective Caml native-code compiler, version %s" v;
          Printf.sprintf "The OCaml native-code compiler, version %s" v] ])

let ocamldep_cmd = (
  [ "ocamldep.opt"], [ "ocamldep" ],
  [
    [ "-version",
      fun v ->
        [ Printf.sprintf "ocamldep, version %s" v]
    ]
  ])

let ocamllex_cmd = (
  [ "ocamllex.opt"], ["ocamllex" ],
  [ [ "-v" ],
    fun v ->
      [ Printf.sprintf "The Objective Caml lexer generator, version %s" v;
        Printf.sprintf "The OCaml lexer generator, version %s" v]
  ])

  let ocamlyacc_cmd = (
    [ "ocamlyacc" ], [],
    [ [ "-version" ],
      fun v -> [
          Printf.sprintf "The Objective Caml parser generator, version %s" v;
          Printf.sprintf "The OCaml parser generator, version %s" v
        ]
    ])

let ocamlmklib_cmd = (
  [], [ "ocamlmklib" ],
  [ [ "-version",
      fun v -> [ Printf.sprintf "ocamlmklib, version %s" v] ]
  ])

  let check_command bindir cmd =
    let file = Filename.concat bindir cmd in
    if Sys.file_exists file then Some file else None

  let rec find_command bindir cmds =
    match cmds with
    | [] -> None
    | cmd :: cmds ->
      let file = check_command bindir cmd in
      match file with
      | Some _ -> file
      | None ->
        let file = check_command bindir (cmd ^ ".exe") in
        match file with
        | Some _ -> file
        | None -> find_command bindir cmds

  let find_ocaml_command switch_version switch_ocamlbin cmd =
    let (cmds_opt, cmds_byte, checkers) = cmd in
    assert false; (* TODO *)
    None

  exception MissingCommand of string
  exception WrongOutput of string list
  let detect_ocaml switch_options =

    let switch_ocamlbin = match switch_options.switch_option_ocamlbin with
      | None -> assert false (* TODO: detect ${toolchain}+ocamlc *)
      | Some ocamlbin -> ocamlbin
    in

    (* 1. find ocamlrun *)
    let switch_ocamlrun =
      match find_command switch_ocamlbin [ "ocamlrun" ] with
      | None -> raise (MissingCommand "ocamlrun (bytecode interpreter)")
      | Some switch_ocamlrun -> switch_ocamlrun in

    (* 1. find ocamlc *)
    let switch_ocamlc =
      match find_command switch_ocamlbin [ "ocamlc.opt"; "ocamlc" ] with
      | None -> raise (MissingCommand "ocamlc variant (bytecode compiler)")
      | Some switch_ocamlc -> switch_ocamlc in

    (* 2. find ocamlc version *)
    let switch_version, switch_ocamlc =
      match get_command_output switch_ocamlc [ "-version" ] with
      | [ switch_version ] -> switch_version, [ switch_ocamlc ]
      | _ ->
        (* try to run ocamlc with ocamlrun *)
        match get_command_output
                switch_ocamlrun [ switch_ocamlc; "-version" ] with
        | [ switch_version ] ->
          switch_version, [ switch_ocamlrun; switch_ocamlc ]
        | _ ->
          raise (WrongOutput (switch_ocamlc :: "-version" :: []))
    in

    (* 3. find ocamlc config *)
    let cfg = match get_config switch_ocamlc with
      | None ->
        raise (WrongOutput (switch_ocamlc @ [ "-config" ]))
      | Some cfg ->
        if cfg.ocaml_version <> switch_version then
          raise (WrongOutput (switch_ocamlc @ [ "-config" ]));
        cfg
    in

    (* 4. find ocamldep *)
    let switch_ocamldep =
      match find_ocaml_command
              switch_version switch_ocamlbin ocamldep_cmd with
      | None ->
        raise (MissingCommand "ocamldep (dependency analyser)")
      | Some switch_ocamldep -> switch_ocamldep in

    let switch_ocamldoc = find_ocaml_command switch_version switch_ocamlbin
        ocamldoc_cmd in
    let switch_ocamlopt = find_ocaml_command switch_version switch_ocamlbin
        ocamlopt_cmd in
    let switch_ocamlyacc = find_ocaml_command switch_version switch_ocamlbin
        ocamlyacc_cmd in
    let switch_ocamllex = find_ocaml_command switch_version switch_ocamlbin
        ocamllex_cmd in
    let switch_ocamlmklib = find_ocaml_command switch_version switch_ocamlbin
        ocamlmklib_cmd in

    let switch_ocamlfind = assert false in  (* TODO *)
    let switch_bindir = assert false in (* TODO *)
    let switch_ocamldir = assert false in (* TODO *)
    let switch_metadir = assert false in (* TODO *)

    Some {
      switch_ocamlbin;
      switch_version;

      switch_ocamlc = switch_ocamlc ;
      switch_ocamlcc = switch_ocamlc;
      switch_ocamldep;
      switch_ocamlrun;

      switch_ocamldoc;
      switch_ocamlopt;
      switch_ocamlyacc;
      switch_ocamlmklib;
      switch_ocamllex;

      switch_ocamllib  = cfg.ocaml_ocamllib;
      switch_version_major = cfg.ocaml_version_major;
      switch_version_minor = cfg.ocaml_version_minor;
      switch_version_point = cfg.ocaml_version_point;
      switch_system = cfg.ocaml_system;
      switch_architecture = cfg.ocaml_architecture;
      switch_ext_obj = cfg.ocaml_ext_obj;
      switch_ext_lib = cfg.ocaml_ext_lib;
      switch_ext_dll = cfg.ocaml_ext_dll;
      switch_os_type = cfg.ocaml_os_type;

      switch_ocamlfind;
      switch_bindir;
      switch_ocamldir;
      switch_metadir;
    }


  let path_sep =
    match Sys.os_type with
      "Win32" -> ';'
    | _ -> ':'

  let split_path path =
    OcpString.split path path_sep

  let search_path sc =
    (try
       [Sys.getenv "OCPBUILD_OCAMLDIR"]
     with Not_found -> []) @
    (try
       split_path (Sys.getenv "OCPBUILD_PATH")
     with Not_found -> []) @
    (try
       split_path (Sys.getenv "OCAMLPATH")
     with Not_found -> []) @
    (match sc.switch_ocamlfind with
     | Some so -> so.ocamlfind_path
     | None ->
       try
         [Sys.getenv "OCAMLFIND_METADIR"]
       with Not_found -> []
    ) @
    [sc.switch_ocamllib]

end

include (IMPLEMENTATION : INTERFACE)

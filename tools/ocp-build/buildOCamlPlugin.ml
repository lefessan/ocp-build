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

open BuildTypes

let plugin =
  let module PluginArg : BuildTypes.PluginArg = struct
    let name = "OCaml"
    let ident = "ocaml"
    let uninstall_path () =
      (* TODO : use detected config *)
      (try
         let opam_prefix = Sys.getenv "OPAM_PREFIX" in
         [Filename.concat opam_prefix "lib"]
       with Not_found -> []) @
      (try
         [Sys.getenv "OCAMLLIB"]
      with Not_found -> [])

  end in
  let module P = BuildPlugins.MakePlugin(PluginArg) in
  P.plugin

(* For now, very simple *)
let create_switch plugin =
  let module Switch : BuildTypes.Switch = struct
    let plugin = plugin
    let ident = ""
    let name = "Default Switch"
  end in
  (module Switch : Switch)

let create_packages = BuildOCamlRules.create plugin

let () =
  BuildPlugins.set_default_plugin plugin

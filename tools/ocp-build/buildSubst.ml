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

(*
TODO:
* BuildOCamlConfig: used to define substitutions for OCAMLLIB and OCAMLBIN.
   Shall we still use them ? Where can we put them ?


let global_subst = StringSubst.Gen.empty_subst ()

let add_variables f env var vv =
  List.fold_left (fun env fmt ->
      f env  (Printf.sprintf fmt var) vv) env [
    "%%{%s}%%";
    "${%s}";
    "$(%s)";
  ]

let add_to_subst env var vv =
  let _env = add_variables (fun env s vv ->
      StringSubst.Gen.add_to_subst env s vv; env) env var vv
  in
  ()

let add_to_global_subst var var_value =
  add_to_subst global_subst var var_value

let _ =
  Array.iter (fun s ->
    let var, var_value = OcpString.cut_at s '=' in
    add_to_global_subst var var_value;
  ) (MinUnix.environment ())

let putenv var var_value =
  MinUnix.putenv var var_value;
  add_to_global_subst var var_value

let subst env_subst s =
  let ss = snd (StringSubst.Gen.iter_subst env_subst s) in
(*  Printf.eprintf "BuildSubst.subst %S -> %S\n%!" s ss; *)
  ss

let subst_global = subst global_subst

let add_to_local_subst env var vv =
  add_variables StringSubst.Gen.add_to_copy env var vv

*)

open StringCompat

let create_substituter list =
  StringMap.of_list list

let apply_substituter subst s info =
  StringSubst.subst (fun s ->
      (StringMap.find s subst) info
    ) s

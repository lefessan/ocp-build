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

open StringCompat
open BuildTypes

let plugins = ref StringMap.empty
let active_plugins = ref StringMap.empty

module MakePlugin(PluginArg: PluginArg) = struct

  module Plugin = struct
    include PluginArg
  end

  let plugin = (module Plugin: Plugin)
  let () =
    plugins := StringMap.add Plugin.name plugin !plugins
end

let dummy_plugin =
  let module P = MakePlugin(struct
      let name = "dummy"
      let ident = name
      let uninstall_path () = []
    end) in
  P.plugin

let default_plugin = ref dummy_plugin
let set_default_plugin p = default_plugin := p

let active_plugins () =
  if StringMap.is_empty !active_plugins then
    [ !default_plugin ]
  else
    StringMap.to_list_of_values !active_plugins

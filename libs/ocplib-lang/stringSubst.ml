(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v2.1 with       *)
(*   the special exception on linking described in the file LICENSE.      *)
(*      (GNU Lesser General Public Licence version 2.1)                   *)
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



(* Fabrice: we use a very basic scheme in O(M.N) where M is the size
   of the file and N the size of the longest key. We store keys in a
   prefix-tree, and search the prefix tree at each file position.

   If we need to be more efficient, we should probably use KMP or
   something like that.
*)

let not_found = Not_found

module CharMap = Map.Make(struct
    type t = char
    let compare x y = Char.code x - Char.code y end)

module Gen = struct

type subst = {
  mutable map : subst CharMap.t;
  mutable result : string option;
}

let rec add_prefix_map key pos v node =
  if pos < String.length key then
    let c = key.[pos] in
    let node =
      try CharMap.find c node.map with Not_found ->
        let new_node = { map = CharMap.empty; result = None } in
        node.map <- CharMap.add c new_node node.map;
        new_node
    in
    add_prefix_map key (pos+1) v node
  else
    node.result <- Some v

let empty_subst () = { map = CharMap.empty; result = None }

let add_to_subst subst key v =
  add_prefix_map key 0 v subst

let add_to_copy subst key v =
  let rec add_to_copy key pos v node =
    if pos < String.length key then
      let c = key.[pos] in
      let new_node =
        try CharMap.find c node.map with Not_found ->
          { map = CharMap.empty; result = None }
      in
      let new_node = add_to_copy key (pos+1) v new_node in
      { node with
        map = CharMap.add c new_node node.map }
    else
      { node with result = Some v }
  in
  add_to_copy key 0 v subst

let subst_of_list list =
  let c = empty_subst () in
  List.iter (fun (key,v) ->
      add_prefix_map key 0 v c;
  ) list;
  c

let rec find_in_map b node s pos =
(*  Printf.fprintf stderr "find_in_map %d/%d\n" pos (String.length s); *)
  if pos < String.length s then
    match node.result with
        None ->
          let node = CharMap.find s.[pos] node.map in
          find_in_map b node s (pos+1)
      | Some repl ->
        try
          let node = CharMap.find s.[pos] node.map in
          find_in_map b node s (pos+1)
        with Not_found ->
          Buffer.add_string b repl;
          pos
  else
    match node.result with
        None -> raise not_found
      | Some repl ->
          Buffer.add_string b repl;
          pos

let subst config s =
  let b = Buffer.create (String.length s) in
  let has_subst = ref 0 in
  let len = String.length s in
  let rec iter pos =
(*    Printf.fprintf stderr "current_pos %d\n" pos; *)
    if pos < len then
      try
        let next_pos = find_in_map b config s pos in
        incr has_subst;
        iter next_pos
      with Not_found ->
          Buffer.add_char b s.[pos];
          iter (pos+1)
  in
  iter 0;
  !has_subst, Buffer.contents b

let iter_subst config s =
  let nsubst = ref 0 in
  let rec iter s =
    let has_subst, s = subst config s in
    nsubst := !nsubst + has_subst;
    if has_subst > 0 then iter s else !nsubst, s
  in
  iter s

end


module M = struct

  type 'a subst = {
    mutable map : 'a subst CharMap.t;
    mutable result : ('a -> string) option;
  }

  let rec add_prefix_map key pos v node =
    if pos < String.length key then
      let c = key.[pos] in
      let node =
        try CharMap.find c node.map with Not_found ->
          let new_node = { map = CharMap.empty; result = None } in
          node.map <- CharMap.add c new_node node.map;
          new_node
      in
      add_prefix_map key (pos+1) v node
    else
      node.result <- Some v

  let empty_subst () = { map = CharMap.empty; result = None }

  let add_to_subst subst key v =
    add_prefix_map key 0 v subst

  let add_to_copy subst key v =
    let rec add_to_copy key pos v node =
      if pos < String.length key then
        let c = key.[pos] in
        let new_node =
          try CharMap.find c node.map with Not_found ->
            { map = CharMap.empty; result = None }
        in
        let new_node = add_to_copy key (pos+1) v new_node in
        { node with
          map = CharMap.add c new_node node.map }
      else
        { node with result = Some v }
    in
    add_to_copy key 0 v subst

  let subst_of_list list =
    let c = empty_subst () in
    List.iter (fun (key,v) ->
      add_prefix_map key 0 v c;
    ) list;
    c

  let rec find_in_map b info node s pos =
    (*  Printf.fprintf stderr "find_in_map %d/%d\n" pos (String.length s); *)
    if pos < String.length s then
      match node.result with
        None ->
        let node = CharMap.find s.[pos] node.map in
        find_in_map b info node s (pos+1)
      | Some repl ->
        try
          let node = CharMap.find s.[pos] node.map in
          find_in_map b info node s (pos+1)
        with Not_found ->
          Buffer.add_string b (repl info);
          pos
    else
      match node.result with
        None -> raise not_found
      | Some repl ->
        Buffer.add_string b (repl info);
        pos

  let subst config s info =
    let b = Buffer.create (String.length s) in
    let has_subst = ref 0 in
    let len = String.length s in
    let rec iter pos =
      (*    Printf.fprintf stderr "current_pos %d\n" pos; *)
      if pos < len then
        try
          let next_pos = find_in_map b info config s pos in
          incr has_subst;
          iter next_pos
        with Not_found ->
          Buffer.add_char b s.[pos];
          iter (pos+1)
    in
    iter 0;
    !has_subst, Buffer.contents b

  let iter_subst config s info =
    let nsubst = ref 0 in
    let rec iter s =
      let has_subst, s = subst config s info in
      nsubst := !nsubst + has_subst;
      if has_subst > 0 then iter s else !nsubst, s
    in
    iter s


end


module Static = struct

  type subst = {
    mutable map : subst CharMap.t;
    mutable result : int option;
  }

  type t = {
    subst_len : int;
    subst_subst : subst;
  }

  let rec add_prefix_map key pos v node =
    if pos < String.length key then
      let c = key.[pos] in
      let node =
        try CharMap.find c node.map with Not_found ->
          let new_node = { map = CharMap.empty; result = None } in
          node.map <- CharMap.add c new_node node.map;
          new_node
      in
      add_prefix_map key (pos+1) v node
    else
      node.result <- Some v

  let empty_subst () = { map = CharMap.empty; result = None }

  let add_to_subst subst key ( v : int ) =
    add_prefix_map key 0 v subst

  let create array =
    let t = {
      subst_len = Array.length array;
      subst_subst = empty_subst ();
    } in
    Array.iteri (fun i s ->
      add_to_subst t.subst_subst s i
    ) array;
    t

      (*
  let add_to_copy subst key v =
    let rec add_to_copy key pos v node =
      if pos < String.length key then
        let c = key.[pos] in
        let new_node =
          try CharMap.find c node.map with Not_found ->
            { map = CharMap.empty; result = None }
        in
        let new_node = add_to_copy key (pos+1) v new_node in
        { node with
          map = CharMap.add c new_node node.map }
      else
        { node with result = Some v }
    in
    add_to_copy key 0 v subst
      *)

      (*
  let subst_of_list list =
    let c = empty_subst () in
    List.iter (fun (key,v) ->
      add_prefix_map key 0 v c;
    ) list;
    c
      *)

  let rec find_in_map b info node s pos =
    (*  Printf.fprintf stderr "find_in_map %d/%d\n" pos (String.length s); *)
    if pos < String.length s then
      match node.result with
        None ->
        let node = CharMap.find s.[pos] node.map in
        find_in_map b info node s (pos+1)
      | Some repl ->
        try
          let node = CharMap.find s.[pos] node.map in
          find_in_map b info node s (pos+1)
        with Not_found ->
          Buffer.add_string b (info.(repl));
          pos
    else
      match node.result with
        None -> raise not_found
      | Some repl ->
        Buffer.add_string b (info.(repl));
        pos

  let subst t info s =
    if Array.length info <> t.subst_len then
      Printf.kprintf failwith "StringSubst.Static.subst invalid sizes %d instead of expected %d" (Array.length info) t.subst_len;

    let b = Buffer.create (String.length s) in
    let has_subst = ref 0 in
    let len = String.length s in
    let rec iter pos =
      (*    Printf.fprintf stderr "current_pos %d\n" pos; *)
      if pos < len then
        try
          let next_pos = find_in_map b info t.subst_subst s pos in
          incr has_subst;
          iter next_pos
        with Not_found ->
          Buffer.add_char b s.[pos];
          iter (pos+1)
    in
    iter 0;
    !has_subst, Buffer.contents b

  let iter_subst t info s =
    let nsubst = ref 0 in
    let rec iter s =
      let has_subst, s = subst t info s in
      nsubst := !nsubst + has_subst;
      if has_subst > 0 then iter s else s
    in
    iter s


end

type delim =
  | DollarParen      (* $(XXX)  *)
  | DollarBrace      (* ${XXX}  *)
  | PercentBrace     (* %{XXX}  *)
  | PercentBraceSym  (* %{XXX}% *)

type error =
  | UnexpectedEndOfString
  | SubstitutionError of string * delim * exn

exception Error of error

let make_delim delim s =
  match delim with
  | DollarParen -> Printf.sprintf "$(%s)" s
  | DollarBrace -> Printf.sprintf "${%s}" s
  | PercentBrace -> Printf.sprintf "%%{%s}" s
  | PercentBraceSym -> Printf.sprintf "%%{%s}%%" s

let string_of_error = function
  | UnexpectedEndOfString -> "Unexpected End of String in Variable"
  | SubstitutionError (s, delim, exn) ->
    Printf.sprintf "Substitution for %s : exception %s"
      (make_delim delim s)
      (Printexc.to_string exn)

let () =
  Printexc.register_printer (fun exn ->
      match exn with
      | Error error ->
        Some (Printf.sprintf "Error(%s)" (string_of_error error))
      | _ -> None
    )

let trysubst delim subst s =
  try subst s with
  | exn ->
    raise (Error (SubstitutionError (s, delim, exn)))

let subst dollarbrace
    ?(dollarparen = dollarbrace)
    ?(percentbrace = dollarbrace)
    ?(percentbracesym = dollarbrace)
    s =
  let len = String.length s in
  let rec iter0 i stack =
    let b = Buffer.create len in
    iter1 b i stack

  and iter1 b i stack =
    if i = len then
      match stack with
      | [] -> Buffer.contents b
      | _ -> raise (Error UnexpectedEndOfString)
    else
      match s.[i], stack with
      | '}', (bb, DollarBrace) :: tailstack ->
        let ss = Buffer.contents b in
        let sss = trysubst DollarBrace dollarbrace ss in
        Buffer.add_string bb sss;
        iter1 bb (i+1) tailstack
      | ')', (bb, DollarParen) :: tailstack ->
        let ss = Buffer.contents b in
        let sss = trysubst DollarParen dollarparen ss in
        Buffer.add_string bb sss;
        iter1 bb (i+1) tailstack
      | '}', (bb, PercentBrace) :: tailstack ->
        let ss = Buffer.contents b in
        if i+1 < len && s.[i+1] = '%' then
          let sss = trysubst PercentBraceSym percentbracesym ss in
          Buffer.add_string bb sss;
          iter1 bb (i+2) tailstack
        else
          let sss = trysubst PercentBrace percentbrace ss in
          Buffer.add_string bb sss;
          iter1 bb (i+1) tailstack
      | c, _ ->
        if i+1 = len then begin
          Buffer.add_char b c;
          iter1 b (i+1) stack
        end else
          let cc = s.[i+1] in
          match c, cc with
          | '$', '{' ->
            iter0 (i+2) ( (b, DollarBrace) :: stack )
          | '$', '(' ->
            iter0 (i+2) ( (b, DollarParen) :: stack )
          | '%', '{' ->
            iter0 (i+2) ( (b, PercentBrace) :: stack )
          | '\\', c ->
            Buffer.add_char b c;
            iter1 b (i+2) stack
          | _ ->
            Buffer.add_char b c;
            iter1 b (i+1) stack
  in
  iter0 0 []


let () =
  let f s = "[" ^ s ^ "," ^ s ^ "]" in

  (*
  let g s =
    let ss = subst f s in
    Printf.eprintf "assert ( subst f %S = %S );\n%!" s ss
  in
  List.iter g [
    "";
    "${}";
    "$()";
    "%{}";
    "%{}%";
    "a${}";
    "a$()";
    "a%{}";
    "a%{}%";
    "${}b";
    "$()b";
    "%{}b";
    "%{}%b";
    "a${}b";
    "a$()b";
    "a%{}b";
    "a%{}%b";
    "a${b%{c}d}e";
    "a$(b%{c}d)e";
    "a%{b%{c}d}e";
    "a%{b%{c}d}%e";
  ];
  exit 2
*)

  assert ( subst f "" = "" );
  assert ( subst f "${}" = "[,]" );
  assert ( subst f "$()" = "[,]" );
  assert ( subst f "%{}" = "[,]" );
  assert ( subst f "%{}%" = "[,]" );
  assert ( subst f "a${}" = "a[,]" );
  assert ( subst f "a$()" = "a[,]" );
  assert ( subst f "a%{}" = "a[,]" );
  assert ( subst f "a%{}%" = "a[,]" );
  assert ( subst f "${}b" = "[,]b" );
  assert ( subst f "$()b" = "[,]b" );
  assert ( subst f "%{}b" = "[,]b" );
  assert ( subst f "%{}%b" = "[,]b" );
  assert ( subst f "a${}b" = "a[,]b" );
  assert ( subst f "a$()b" = "a[,]b" );
  assert ( subst f "a%{}b" = "a[,]b" );
  assert ( subst f "a%{}%b" = "a[,]b" );
  assert ( subst f "a${b%{c}d}e" = "a[b[c,c]d,b[c,c]d]e" );
  assert ( subst f "a$(b%{c}d)e" = "a[b[c,c]d,b[c,c]d]e" );
  assert ( subst f "a%{b%{c}d}e" = "a[b[c,c]d,b[c,c]d]e" );
  assert ( subst f "a%{b%{c}d}%e" = "a[b[c,c]d,b[c,c]d]e" );
  ()

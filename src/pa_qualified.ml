
(* Pa_qualified
   -----------------------------------------------------------------------------
   Copyright (C) 2015, Max Mouratov (mmouratov@gmail.com)

   License:
     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Library General Public
     License version 2.1, as published by the Free Software Foundation.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

     See the GNU Library General Public License version 2.1 for more details
     (enclosed in LICENSE.txt).

   Description:
     Pa_qualified adds support for fully qualified module references to OCaml.
     If a module reference (in any possible context) starts with "Q.", then
     the rest of the reference denotes a context-independent globally unique
     path (as if the reference was located at the very beginning of the file).
     Qualified references can never be shadowed by other definitions
     (warranty void if "Q" is defined explicitly somewhere).

     See README.rst for more information.
*)


open Camlp4


module StringMap = Map.Make (String)

module Id: Sig.Id = struct
  let name = "pa_qualified"
  let version = "0.6"
end


(* The predefined prefix that denotes a globally qualified name *)
let qualified_prefix = "Q"


module Make (AstFilters: Camlp4.Sig.AstFilters) =
struct
  open AstFilters


  (* Generating a globally unique name for the helper module *)
  let gen_helper_name loc =
    let fname = Filename.chop_extension (Ast.Loc.file_name loc) in
    Printf.sprintf "_Q_%s_" fname


  (* Replacing all Qs with the unique name of the helper module,
     as well as collecting all the different Xs in Q.X.* references *)
  let make_reference_collector helper_name =
    object (self) inherit Ast.map as super

      (* A map of globally referenced modules (name -> location) *)
      val mutable collected: Ast.Loc.t StringMap.t =
        StringMap.empty

      method get_collected =
        StringMap.bindings collected

      method ident id =
        match id with

          (* Getting the X out of Q.X.*,
             replacing Q with a reference to the helper module *)
          | IdAcc _ ->
              (match Ast.list_of_ident id [] with
                | IdUid (head_loc, head) :: ((IdUid (x_loc, x) :: _) as rest)
                  when head = qualified_prefix ->
                    collected <- StringMap.add x x_loc collected;
                    Ast.idAcc_of_list (IdUid (head_loc, helper_name) :: rest)

                | _ -> id)

          | _ ->
              super#ident id

      end


  (* Injecting the helper module into the implementation *)
  let () =
    AstFilters.register_str_item_filter (fun si ->
      let _loc = Ast.loc_of_str_item si in
      let helper_name = gen_helper_name _loc in
      let collector = make_reference_collector helper_name in
      let si = collector#str_item si in
      let qualified = collector#get_collected in
      match qualified with
        | [] ->
            si
        | ids ->
            <:str_item<
              module $uid:(helper_name)$ = struct
                $list:(ids |> List.map (fun (id, _loc) ->
                         <:str_item<
                           module $uid:(id)$ = $uid:(id)$
                         >>))$
              end;
              $(si)$;
            >>)


  (* Injecting the helper module into the interface *)
  let () =
    AstFilters.register_sig_item_filter (fun si ->
      let _loc = Ast.loc_of_sig_item si in
      let helper_name = gen_helper_name _loc in
      let collector = make_reference_collector helper_name in
      let si = collector#sig_item si in
      let qualified = collector#get_collected in
      match qualified with
        | [] ->
            si
        | ids ->
            <:sig_item<
              module $uid:(helper_name)$: sig
                $list:(ids |> List.map (fun (id, _loc) ->
                         <:sig_item<
                           module $uid:(id)$: module type of $uid:(id)$
                         >>))$
              end;
              $(si)$;
            >>)

end


module M = Camlp4.Register.AstFilter (Id) (Make)


(* Pa_qualified
   -----------------------------------------------------------------------------
   Copyright (C) 2015, Max Mouratov

   License:
     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Library General Public
     License version 2.1, as published by the Free Software Foundation.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

     See the GNU Library General Public License version 2.1 for more details
     (enclosed in the file LICENSE.txt).

   Description:
     Pa_qualified adds support for fully qualified module references to OCaml.
     If a module reference (in any possible context) starts with "Q.", then
     the rest of the reference denotes a context-independent globally unique
     path (as if the reference was located at the very beginning of the file).
     Qualified references can never be shadowed by other definitions
     (warranty void if "Q" is defined explicitly somewhere).

   Implementation:
     The extension works by injecting a uniquely named helper module at the
     beginning of the module that is being processed.

     An example that explains everything:

     Was:

       ...

       Q.List.map

       ...

       Q.Scanf.Scanning.bscanf


     Becomes:

       module _Q_filename_ = struct
         List = List
         Scanf = Scanf
       end

       ...

       _Q_filename_.List.map

       ...

       _Q_filename_.Scanf.Scanning.bscanf

*)


open Camlp4


module StringSet = Set.Make (String)

module Id: Sig.Id = struct
  let name = "pa_qualified"
  let version = "0.5"
end


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

      (* A set of globally referenced modules *)
      val mutable collected = StringSet.empty
      method get_collected =
        StringSet.elements collected

      method ident id =
        match id with

          (* Getting the X out of Q.X.*,
             replacing Q with a reference to the helper module *)
          | IdAcc _ ->
              (match Ast.list_of_ident id [] with
                | IdUid (head_loc, head) :: ((IdUid (_, x) :: _) as rest)
                  when head = qualified_prefix ->
                    collected <- StringSet.add x collected;
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
                $list:(ids |> List.map (fun id ->
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
            let _loc = Ast.loc_of_sig_item si in
            <:sig_item<
              module $uid:(helper_name)$: sig
                $list:(ids |> List.map (fun id ->
                         <:sig_item<
                           module $uid:(id)$: module type of $uid:(id)$
                         >>))$
              end;
              $(si)$;
            >>)

end


module M = Camlp4.Register.AstFilter(Id)(Make)

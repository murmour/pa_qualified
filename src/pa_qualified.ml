
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
     The extension works by injecting a helper module Q at the beginning of
     the module that is being processed. An example that explains everything:

       module Q = struct
         List = List
         Scanf = Scanf
       end

       ...

       Q.List.map

       ...

       Q.Scanf.Scanning.bscanf

*)


open Camlp4


module Id: Sig.Id = struct
  let name = "pa_qualified"
  let version = "0.5"
end


let helper_module_name = "Q"


module Make (AstFilters: Camlp4.Sig.AstFilters) =
struct
  open AstFilters


  (* Collecting all the different Xs in Q.X.* references *)
  let qualified_reference_collector =
    let module SSet = Set.Make (String) in
    object (self) inherit Ast.fold as super
      val fv = SSet.empty
      method fv = SSet.elements fv
      method ident = function

        (* Getting the X out of Q.X.* *)
        | IdAcc (_, IdUid (_, head), rest) when head = helper_module_name ->
            (match Ast.list_of_ident rest [] with
              | IdUid (_, x) :: _ ->
                  {< fv = SSet.add x fv >}
              | _ ->
                  self)

        (* Module applications can contain Q.X.* *)
        | IdApp _ as id ->
            super#ident id

        | etc ->
            self
    end


  (* Injecting the helper module into the implementation *)
  let () =
    AstFilters.register_str_item_filter (fun si ->
      let qualified = (qualified_reference_collector#str_item si)#fv in
      match qualified with
        | [] ->
            si
        | ids ->
            let _loc = Ast.loc_of_str_item si in
            <:str_item<
              module $uid:(helper_module_name)$ = struct
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
      let qualified = (qualified_reference_collector#sig_item si)#fv in
      match qualified with
        | [] ->
            si
        | ids ->
            let _loc = Ast.loc_of_sig_item si in
            <:sig_item<
              module $uid:(helper_module_name)$: sig
                $list:(ids |> List.map (fun id ->
                         <:sig_item<
                           module $uid:(id)$: module type of $uid:(id)$
                         >>))$
              end;
              $(si)$;
            >>)

end


module M = Camlp4.Register.AstFilter(Id)(Make)

============
Pa_qualified
============

Pa_qualified adds support for fully qualified module references to OCaml.
If a module reference (in any possible context) starts with "Q.", then
the rest of the reference denotes a context-independent globally unique
path (as if the reference was located at the very beginning of the file).
Qualified references can never be shadowed by other definitions
(warranty void if "Q" is defined explicitly somewhere).

See the file INSTALL.txt for building and installation instructions.
See the file LICENSE.txt for copying conditions.

Home page: https://github.com/cakeplus/pa_qualified


Usage example
=============

.. sourcecode:: ocaml

  open Batteries

  (* A function from Batteries.List *)
  let fn1 =
    List.length

  (* A function from List of the OCaml distribution *)
  let fn2 =
    Q.List.length

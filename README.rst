============
Pa_qualified
============

Pa_qualified adds support for fully qualified module references to OCaml.
If a module reference (in any possible context) starts with "Q.", then
the rest of the reference denotes a context-independent globally unique
path (as if the reference was located at the very beginning of the file).
Qualified references can never be shadowed by other definitions
(warranty void if "Q" is defined explicitly somewhere).

See INSTALL.txt for building and installation instructions.
See LICENSE.txt for copying conditions.

Home page: https://github.com/cakeplus/pa_qualified


Implementation
==============

The syntax extension works by injecting a uniquely named helper module at
the beginning of the module that is being processed.

An example that explains everything:

Was:

.. sourcecode:: ocaml

  (* ... *)

  Q.List.map

  (* ... *)

  Q.Scanf.Scanning.bscanf


Becomes:

.. sourcecode:: ocaml

  module _Q_filename_ = struct
    List = List
    Scanf = Scanf
  end

  (* ... *)

  _Q_filename_.List.map

  (* ... *)

  _Q_filename_.Scanf.Scanning.bscanf


Usage example
=============

.. sourcecode:: ocaml

  open Batteries

  (* A function from Batteries.List *)
  let fn1 =
    List.length

  (* A function from List of the OCaml standard library *)
  let fn2 =
    Q.List.length

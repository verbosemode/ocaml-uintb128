# uintb128

**unreleased**

* Library for 128 bit unsigned integers
* Based on OCaml's [Bytes](https://v2.ocaml.org/api/Bytes.html) implementation of the standard library.
* Created initially as a playground for ...
  - ocaml-ipaddr issue [16](https://github.com/mirage/ocaml-ipaddr/issues/16) - Use 16-byte string for internal representation of IPv6 addresses
  - Merged in [#115](https://github.com/mirage/ocaml-ipaddr/pull/115) - use Bytes.t as data type for IPv6 addresses

## TODO

* add mli and documentation
* add string integer conversion
* add infix module like integers
  - separate operators for bitwise operations?
* installable top level printer

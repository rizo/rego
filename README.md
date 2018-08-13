# rego

Reasonable Go.

## Status

This project is an experimental ongoing attempt at creating a Go backend for
Reason/OCaml. Currently a proof of concept compiler is being developed that
operates on the OCaml's Lambda IR and produces textual Go code. This approach
poses a number of challenges with regards to performance: all values are
currently represented as empty interfaces, which results in a significant
overhead in the generated Go code (see [golang/go#20116](https://github.com/golang/go/issues/20116)
for details).


## Development

The code is incomplete and mostly broken. If you wish to contribute, please [open an issue](https://github.com/rizo/rego/issues).



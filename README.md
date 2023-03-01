# Build Your Own Probabilistic Programming Language

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/gbdrt/byo-ppl/HEAD)

## Using OCaml 5

Create a switch with ocaml 5 :

```
opam switch create 5.0.0~rc1
````

You can then follow the normal procedure.

## Install

After cloning the repo, the easiest way to install all the dependencies is via opam:
```
git clone https://github.com/samsa1/byo-ppl-22-23.git
cd byo-ppl
opam install . --deps-only
```

You can then test your installation with a simple:

```
dune build
```

Or try an example with:
```
dune exec ./examples/funny_bernoulli.exe
```

## Organization

The `Byoppl` library contains the following modules

- `Distribution`: Library of probability distributions and basic statistical functions.
- `Basic` (TODO): Basic inference with rejection sampling and importance sampling.
- `Infer` (TODO): Inference on Continuation Passing Style (CPS) models.
- `Cps_operators`: Syntactic sugar to write CPS style probabilistic models.
- `Utils`: Missing utilities functions used in other modules.

Examples can be found in the `examples` directory.

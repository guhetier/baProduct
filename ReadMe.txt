# baProduct

## What is it ?

baProduct is a formal verification tool for concurrent C programs (POSIX API).
It allows to prove specification following the formalism introduced
[here](https://github.com/xNephe/memoire_maitrise) (in French).

In a few words, this formalism of specification allows to specify LTL properties
using local and global variables and position in the program. Atomic
propositions using local variables are correctly defined in the whole program
using the concept of *validity area* and default values. The specification is
expressed in JSON. Examples may be found in the *test* folder.

If you are interested in this tool and / or specification formalism and do not
read French, please do contact me and I will do my possible to improve English
documentation.

## How does it works ?

baProduct implemented a source to source transformation of the program sources.
It builds the product between the program and a Büchi automaton representing the
specified LTL property, in C. Error states are then specified using assertions.

A model-checker is used as a backend to perform the actual verification task.
baProduct is currently compatible with ESBMC and CBMC.

## Dependencies

baProduct is implemented in OCaml. It uses the following packages :

- CIL
- OCamlGraph
- str
- yojson

The most easy way to install them is using Opam.
The compilation process use ocamlbuild.

baProduct use LTL2BA to build the Büchi automaton associated with a LTL formula.
A slightly modified version is used, in order to output the automaton in JSON
(and not in Promela as in the original version). This version can be found
[here](https://github.com/xNephe/ltl2ba).

## Usage

The easiest way to use baProduct is through the wrapper *script/baProduct.py*.
It automatically preprocess sources and launch baProduct with the most common
set of parameters.

```
# script/baProduct.py -s specification_file -i input_sources.c -o output_sources.c
```

To run the tests, the script *script/run_test.py* is available.

```
# script/run_test.py {test_name}
```

where {test_name} is the name of a subfolder in *test/* respecting the
convention already used in the folder.

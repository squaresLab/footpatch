# Install

The basic procedure is:

Download the pre-build clang version of Infer v0.9.3 to "SOME_DIR"

`https://github.com/facebook/infer/releases/download/v0.9.3/infer-linux64-v0.9.3.tar.xz`

Go to "SOME_DIR", do `./configure`, `make` (assuming opam switch/dependencies are installed with `build-infer.sh`)


## The for footpatch:

Put "SOME_DIR" in `CONFIG`

Run `MERGE.sh`, which copies footpatch and other files

Do `./configure` and `make` again

Test it by going to `patching/test/null-deref-java` and run `make test`

## Misc:

- `make test` above will make sure that `rename` is compiled. It then invokes
`autopatch.sh`. 

- a `sed` command has to replace placeholders once everything is copied to a 
fresh infer install for: (1) the rename command invoked in `footpatch_utils.ml`
and (2) the path that `rename` must `cd` to because CLASSPATH is relative.


## Deps

```
# Installed packages for infer-4.02.3:
ANSITerminal          0.7  Basic control of ANSI compliant terminals and the windows shell.
atd                 1.2.0  Parser for the ATD data format description language
atdgen             1.10.0  Generates efficient JSON serializers, deserializers and validators
base-bigarray        base  Bigarray library distributed with the OCaml compiler
base-bytes           base  Bytes library distributed with the OCaml compiler
base-ocamlbuild      base  OCamlbuild binary and libraries distributed with the OCaml compiler
base-threads         base  Threads library distributed with the OCaml compiler
base-unix            base  Unix library distributed with the OCaml compiler
BetterErrors        0.0.1  Better compiler error output.
biniou             1.0.12  Binary data format designed for speed, safety, ease of use and backward compatibility as protocols evolve
camlp4             4.02+7  Camlp4 is a system for writing extensible parsers for programming languages
camlzip              1.05  Provides easy access to compressed files in ZIP, GZIP and JAR format
camomile            0.8.5  A comprehensive Unicode library
conf-autoconf         0.1  Virtual package relying on autoconf installation.
conf-m4                 1  Virtual package relying on m4
conf-ncurses            1  Virtual package relying on ncurses
conf-pkg-config       1.0  Virtual package relying on pkg-config installation.
conf-which              1  Virtual package relying on which
cppo                1.4.1  Equivalent of the C preprocessor for OCaml programs
easy-format         1.2.0  High-level and functional interface to the Format module of the OCaml standard library
extlib              1.5.4  A complete yet small extension for OCaml standard library
javalib             2.3.1  Javalib is a library written in OCaml which aims at providing a high level representation of Java .class files.
lambda-term        1.10.1  Terminal manipulation library for OCaml
lwt                 2.4.5  A cooperative threads library for OCaml
menhir           20170101  LR(1) parser generator
merlin              2.3.1 (pinned)  Editor helper, provides completion, typing and source browsing in Vim and Emacs
merlin_extend         0.2 (pinned)  
ocamlbuild              0  Build system distributed with the OCaml compiler since OCaml 3.10.0
ocamlfind           1.7.1  A library manager for OCaml
ounit               2.0.0  Unit testing framework loosely based on HUnit. It is similar to JUnit, and other XUnit testing frameworks
re                  1.7.1  RE is a regular expression library for OCaml
react               1.2.0  Declarative events and signals for OCaml
reason              0.0.5 (pinned)  Reason: Meta Language Toolchain
sawja               1.5.1  Provide a high level representation of Java bytecode programs and static analysis tools.
utop               1.19.3  Universal toplevel for OCaml
yojson              1.3.3  Yojson is an optimized parsing and printing library for the JSON format 
zed                   1.4  Abstract engine for text edition in OCaml
```

```
rvt:~/ $ opam pin list                                                                                                                                                  [3:12:22]
infer-deps-9FN6.0.9.3   (uninstalled)  path  /home/rvt/infer-linux64-v0.9.3/infer-deps-9FN6              
infer-deps-NFk6.0.9.3   (uninstalled)  path  /home/rvt/infer-linux64-v0.9.3/infer-deps-NFk6              
infer-deps-UeYt.0.9.3   (uninstalled)  path  /home/rvt/infer-linux64-v0.9.3/infer-deps-UeYt              
infer-deps-XkOt.0.9.3   (uninstalled)  path  /home/rvt/infer-linux64-v0.9.3/infer-deps-XkOt              
merlin.2.3.1                           git   https://github.com/the-lambda-church/merlin.git#reason-0.0.1
merlin_extend.0.2                      git   https://github.com/let-def/merlin-extend.git#reason-0.0.1   
reason.0.0.5                           git   https://github.com/jberdine/reason.git#infer 
```

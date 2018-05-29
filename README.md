# Build from source

If you just want to play with running footpatch (and possibly hacking at the code),
use the [VM](#vm-download). Dependencies for olders versions of Infer can (and have) broken or
disappeared, and can cause building from source to be problematic. 

If you really want to build from source, here is some guidance.
Building is only tested on Ubuntu 14.04 and Infer v0.9.3. There is
no support for other versions of Infer at this time. Install the [dependencies](#depencies-)

If you have all the dependencies installed, running `./SETUP.sh` should be
enough.

# VM Download

Download each of `footpatch-*` and run `cat footpatch-aa footpatch-ab footpatch-ac footpatch-ad > footpatch.vmdk`. Add the vmdk image in VirtualBox or VMware. Username/password is `vagrant/vagrant`.

# Dependencies

You need at least the following dependencies:

```
# install opam
add-apt-repository ppa:avsm/ppa
apt-get update
apt-get install opam

# additional deps
opam depext conf-autoconf.0.1
opam depext conf-m4.1
opam depext camlzip.1.05

# java
sudo apt-get install default-jdk
sudo apt-get install default-jre

```

For projects tested, you need to do at least:

```

sudo add-apt-repository ppa:ubuntu-toolchain-r/test 
sudo apt-get update
sudo apt-get install --only-upgrade libstdc++6

sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get install oracle-java8-installer
sudo apt-get install gradle

sudo apt-get install clang
sudo apt-get install subversion
```

## Known good package versions

These package versions are known to be good for the source build release, but
may have updated constraints in opam that make it hard to obtain this configuration.

```
# Installed packages for infer-4.02.3:                                              
ANSITerminal              0.7  Basic control of ANSI compliant terminals and the
atd                     1.2.0  Parser for the ATD data format description langua
atdgen                 1.10.0  Generates efficient JSON serializers, deserialize
base-bigarray            base  Bigarray library distributed with the OCaml compi
base-bytes               base  Bytes library distributed with the OCaml compiler
base-ocamlbuild          base  OCamlbuild binary and libraries distributed with 
base-threads             base  Threads library distributed with the OCaml compil
base-unix                base  Unix library distributed with the OCaml compiler 
BetterErrors            0.0.1  Better compiler error output.                        
bin_prot            113.33.03  A binary protocol generator                          
biniou                 1.0.12  Binary data format designed for speed, safety, ea
camlp4                 4.02+7  Camlp4 is a system for writing extensible parsers
camlzip                  1.05  Provides easy access to compressed files in ZIP, 
camomile                0.8.5  A comprehensive Unicode library                      
conf-autoconf             0.1  Virtual package relying on autoconf installation.
conf-m4                     1  Virtual package relying on m4                        
conf-ncurses                1  Virtual package relying on ncurses                   
conf-pkg-config           1.0  Virtual package relying on pkg-config installatio
conf-which                  1  Virtual package relying on which                     
core                113.33.03  Industrial strength alternative to OCaml's standa
core_kernel         113.33.03  Industrial strength alternative to OCaml's standa
cppo                    1.4.1  Equivalent of the C preprocessor for OCaml progra
ctypes                 0.12.1  Combinators for binding to C libraries without wr
ctypes-foreign          0.4.0  Virtual package for enabling the ctypes.foreign s
easy-format             1.2.0  High-level and functional interface to the Format
extlib-compat           1.7.2  A complete yet small extension for OCaml standard
fieldslib           113.33.03  Syntax extension to define first class values rep
integers                0.2.2  Various signed and unsigned integer types for OCa
javalib                 2.3.3  Javalib is a library written in OCaml with the ai
js-build-tools      113.33.04  Collection of tools to help building Jane Street 
lambda-term            1.10.1  Terminal manipulation library for OCaml              
lwt                     2.4.5  A cooperative threads library for OCaml              
menhir               20170101  LR(1) parser generator                               
merlin                  2.5.5  Editor helper, provides completion, typing and so
merlin-extend             0.3  A protocol to provide custom frontend to Merlin  
merlin_extend             0.2 (pinned)                                              
ocamlbuild                  0  Build system distributed with the OCaml compiler 
ocamlfind               1.7.1  A library manager for OCaml                          
ounit                   2.0.0  Unit testing framework loosely based on HUnit. It
ppx_assert          113.33.03  Assert-like extension nodes that raise useful err
ppx_bench           113.33.03  Syntax extension for writing in-line benchmarks i
ppx_bin_prot        113.33.03  Generation of bin_prot readers and writers from
ppx_compare         113.33.03  Generation of comparison functions from types    
ppx_core            113.33.03  Standard library for ppx rewriters               
ppx_custom_printf   113.33.03  Printf-style format-strings for user-defined stri
ppx_deriving              4.1  Type-driven code generation for OCaml >=4.02     
ppx_driver          113.33.04  Feature-full driver for OCaml AST transformers   
ppx_enumerate       113.33.03  Generate a list containing all values of a finite
ppx_expect          113.33.03  Cram like framework for OCaml                    
ppx_fail            113.33.03  Add location to calls to failwiths               
ppx_fields_conv     113.33.03  Generation of accessor and iteration functions fo
ppx_here            113.33.03  Expands [%here] into its location                
ppx_inline_test     113.33.03  Syntax extension for writing in-line tests in oca
ppx_jane            113.33.03  Standard Jane Street ppx rewriters               
ppx_let             113.33.03  Monadic let-bindings                             
ppx_optcomp         113.33.03  Optional compilation for OCaml                   
ppx_pipebang        113.33.03  A ppx rewriter that inlines reverse application o
ppx_sexp_conv       113.33.03  Generation of S-expression conversion functions f
ppx_sexp_message    113.33.03  A ppx rewriter for easy construction of s-express
ppx_sexp_value      113.33.03  A ppx rewriter that simplifies building s-express
ppx_tools          5.0+4.02.0  Tools for authors of ppx rewriters and other synt
ppx_type_conv       113.33.03  Support Library for type-driven code generators  
ppx_typerep_conv    113.33.03  Generation of runtime types from type declaration
ppx_variants_conv   113.33.03  Generation of accessor and iteration functions fo
re                      1.7.1  RE is a regular expression library for OCaml     
react                   1.2.0  Declarative events and signals for OCaml         
reason                  1.4.0  Reason: Meta Language Toolchain                  
result                    1.2  Compatibility Result module                      
sawja                   1.5.2  Provide a high level representation of Java bytec
sexplib             113.33.03  Library for serializing OCaml values to and from 
topkg                   0.9.0  The transitory OCaml software packager           
typerep             113.33.03  typerep is a library for runtime types.          
utop                   1.19.3  Universal toplevel for OCaml                     
variantslib         113.33.03  Part of Jane Street's Core library               
xmlm                    1.3.0  Streaming XML codec for OCaml                    
yojson                  1.3.3  Yojson is an optimized parsing and printing libra
zed                       1.4  Abstract engine for text edition in OCaml
```

```
merlin_extend.0.2                      git   https://github.com/let-def/merlin-extend.git#reason-0.0.1
```

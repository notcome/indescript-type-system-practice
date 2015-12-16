# Indescript: type system practice
With my experience in the last quarter, It is clear that I lack both a concrete
understanding of type inference algorithm and experience in developing “real
world” Haskell programs. As a result, I create this repository dedicated to
playing prototypes of inference algorithms. Here is the list of scenarios I have
worked on:

1. Simple kind inference. This is a simplification of the original Damas-Milner
algorithm. Here I focus on how to use side-effects—algorithm W—to effectively
unify constraints. I heavily consult the paper _Practical type inference for
arbitrary-rank types_ by Simon Peyton Jones et al.

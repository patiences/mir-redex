# mir-redex

This is an attempt to model the [operational semantics](https://en.wikipedia.org/wiki/Operational_semantics)
of the [intermediate representation inside the Rust compiler (MIR)](https://github.com/rust-lang/rust/tree/master/src/librustc/mir).
The modelling language used is [Redex](https://redex.racket-lang.org/).

# Overview

The idea is that we should be able to take the MIR of a Rust program,
convert that into the equivalent Racket form as defined by the
*grammar* in this model, and then *run* that program according to the *reduction rules*.
The great thing about Redex and reduction rules is that we'll be able to look at all
the individual steps that the program took, not just the end result, which is what
operational semantics is all about.  

This is an undergraduate research project I am working on with [Ron Garcia](http://www.cs.ubc.ca/~rxg/) at
the University of British Columbia to learn more about PL and is a work in progress! :-)  

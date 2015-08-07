# Vegas compiler

[![Build Status](https://travis-ci.org/rrdelaney/vegas.svg?branch=master)](https://travis-ci.org/rrdelaney/vegas)
[![Codacy Badge](https://www.codacy.com/project/badge/1abbaf9cfe93479ebf539e2095b31ff5)](https://www.codacy.com/app/rrdelaney/vegas)
[![Coverage Status](https://coveralls.io/repos/rrdelaney/vegas/badge.svg?branch=master&service=github)](https://coveralls.io/github/rrdelaney/vegas?branch=master)

A language that compiles to PHP lol

Now with type-dependant macros!

## Examples

```
// "Notice how comments are interpreted too"
let a = 0

// "Inline comments are not allowed"
let b = 3

// "The compiler determines the type of `c` to be `Number`"
let c = a + b

/**
/* "The syntax for calling PHP functions is similar"
/* "to Rust's macro syntax"
/*
print!(a)

// "Immutability is enforced, these statements error"
let d = "jim"
let d = "pam"

// "Mutable variables"
let mut e = "dwight"
let e = "ryan"
```

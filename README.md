# Vegas compiler

[![Build Status](https://travis-ci.org/rrdelaney/vegas.svg?branch=master)](https://travis-ci.org/rrdelaney/vegas)
[![Codacy Badge](https://www.codacy.com/project/badge/1abbaf9cfe93479ebf539e2095b31ff5)](https://www.codacy.com/app/rrdelaney/vegas)
[![Coverage Status](https://coveralls.io/repos/rrdelaney/vegas/badge.svg?branch=master&service=github)](https://coveralls.io/github/rrdelaney/vegas?branch=master)

A language that compiles to PHP lol

Now with compile-time macros!

## Examples

```
let a = 0
print a
```

```
let myArray = [1, 2, 4]
let [a, b] = myArray
```

```
let m = if on then "ayy lmao" else "rawr"

let k = 17
k = 0 # ERROR! IMMUTIBILITY!

let mut r = 17
r = 0 # No error, declared immutable
```

# Developing

To build the compiler run

```
sbt assembly
mv target/scala-2.11/vegasc-assembly-1.0.jar ./vegasc
```

and then run with

```
java -jar vegasc
```

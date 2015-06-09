# Vegas compiler

A language that compiles to PHP lol

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
sbt package
mv target/scala-2.11/vegasc-assembly-1.0.jar ./vegasc
```

and then run with

```
scala vegasc
```

## Compiler Stages

### Comment

The Comment compiler destroys all comments not in the form `#[note] args`,
that is used for compiler hints. Comments take the form of anything following a
`#` on a line.

### Braces

The Braces Compiler manages braces insertion. This makes the grammar context
free for all following stages, making parsing a lot easier. Currently, the
rules for adding a brace onto the current line are:

- `{` The current line starts with 4 more spaces than the previous
- `}` The current line has 4+ less spaces than the previous line

### Semicolon

The Semicolon Compiler inserts semicolons to make parsing expressions easier.
The current semicolon insertion rules are:

- Every line has a semicolon on the the except
    + The line starts with `.`
    + The previous line ends with `+`
    + The previous line ends with `-`
    + The previous line ends with `=`
    + The current line doesn't contain a `:`

### Stage N

Stage N is a yet undetermined stage because I don't know how many more stages
are going to come before it. Stage N parses everything with a PEG and generates
PHP with the Parboiled2 library.

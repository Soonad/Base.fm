# Formality-Base Style Guide

## General

- Lines must be 80 columns or fewer in length
- names of Types use camelCase
- names of terms use snake_case
- names of proofs may use dot.case
- Tabs are forbidden
- 2 spaces per indent level

## Abstract data types and Records

Contents of an ADT must either on 1 line per variant:

```javascript
T Magma {A : Type}
| magma { f : A -> A -> A }
```

or each variant must open a new indent level, with the brackets and commas
aligned:

```javascript
T Group {A : Type}
| group
  { f           : A -> A -> A
  , e           : A
  , associative : Associative(A,f)
  , identity    : Identity(A,f,e)
  , inverse     : Inverse(A,f,e,identity)
  }
```

The `:` in the type signatures must also align.

# Multiline functions

Functions may be written with one argument per line like so:

```
apply(
  ~A
, ~C
, apply(
    ~V
  , ~AC
  , apply(~U, ~V -> AC, pure(~(U -> V -> AC), F/compose(~A,~B,~C)), u)
  , v
  )
, w
)
```



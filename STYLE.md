# Base.fm Style Guide


> "considering making single-line definitions with `;` a syntax error to stop
> your kingdom of evil"
> --Victor Maia

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL
NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and
"OPTIONAL" in this document are to be interpreted as described in
[RFC 2119](https://tools.ietf.org/html/rfc2119)

## General

- Lines must be 80 columns or fewer in length
- Code content in lines should be 60 columns or fewer in length
- Tabs are forbidden
- 2 spaces per indent level

## Definitions

### Types

A "type constant" is a Formality expression of type `Type`, i.e. an expression
`t` for which, `t :: Type` is valid.

A "type constructor" is a Formality expression of type `X -> T`, where `X` is
any type and `T` is a type constructor or a type constant.

In other words

```
X1 -> Type
X1 -> X2 -> Type
X1 -> X2 -> X3 -> Type
X1 -> .... -> Xn -> Type
```

are all type constructors.

The word "type" in this document shall be used to describe either type constants
or type constructors

### Terms

A term is a Formality expression `t :: A`, where `A` is not a type.

## Capitalization and Case of names

- type names must use BactrianCase (initial upper case CamelCase)
- term names must use snake_case, dot.case or dromedaryCase (initial lower
  case camelCase)
  - snake_case should be used as the standard "word" separator
  - dot.case should be used to signify inheritance or logical containment
  - dromedaryCase should be used for terms that are not executable at runtime
    and are used only at the type level (e.g. `emptyTerm :: Empty`)

## Abstract data types and Records

Contents of an ADT must either be on 1 line per variant:

```javascript
T ADT<A, B>
| adt1(adt1A : A, adt1B : B)
| adt2(adt2A : A, adt2B : B)
```

or each variant must open a new indent level, with the closing `)` on its own
line at the same level as the variant constructor:

```javascript
T ADT<A,B>
| adt1(
    adt1A : A,
    adt1B : B
  )
| adt2(
    adt2A : A,
    adt2B : B
  )
```

The `:` in the type signatures must align within each variant, and should align
across variants.

## Module conventions

Modules *MUST* contain a header containing an abstract description of the file's
contents.

For `And.fm`:

```javascript
// And.fm defines the logical conjunction or product type
//
// A value (proof) of type `And(A,B)` contains *both*
// - a value (proof) of the first type, `A`, *and*...
// - a value (proof) of the second type, `B`
```

- A function whose primary purpose is to convert between `A : Type` and `B :
  Type` should be named `A_to_B`





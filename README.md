# Set Operations Interpreter in OCaml

## Project Overview

This project implements an interpreter for a functional language extended with set manipulation capabilities in OCaml. The implementation includes both dynamic and static type checking approaches to validate operations on sets containing integers or strings.

## Features

The interpreter supports the following set operations:

### Set Creation
- `Empty(t)` - Creates an empty set of type `t`
- `Singleton(v, t)` - Creates a set with a single value `v` of type `t`
- `Of(t, n, from)` - Creates a set containing values from `from+1` to `n-1` of type `t`

### Basic Set Operations
- `Union(set1, set2)` - Returns the union of two sets
- `Inter(set1, set2)` - Returns the intersection of two sets
- `Diff(set1, set2)` - Returns the difference between sets

### Element Operations
- `Insert(elem, set)` - Inserts an element into a set
- `Delete(elem, set)` - Removes an element from a set
- `IsEmpty(set)` - Checks if a set is empty
- `IsIn(elem, set)` - Checks if an element belongs to a set
- `IsSubset(set1, set2)` - Checks if set1 is a subset of set2
- `Min(set)` - Returns the minimum value in a set
- `Max(set)` - Returns the maximum value in a set

### Functional Operations
- `For_all(predicate, set)` - Checks if all elements satisfy the predicate
- `Exists(predicate, set)` - Checks if at least one element satisfies the predicate
- `Filter(predicate, set)` - Returns a set of elements satisfying the predicate
- `Map(function, set)` - Applies a function to each element, returning a new set

## Project Structure

```
.
├── dynamic/
│   ├── expModule.ml      # Core implementation with dynamic type checking
│   └── testCase.ml       # Test cases for dynamic type checking
├── static/
│   ├── withStaticTypeCheck.ml      # Implementation with static type checking
│   └── TestCaseForStaticChecker.ml # Test cases for static type checking
├── README.md             # This file
├── relazione.pdf         # Project report (Italian)
└── semantica.pdf         # Semantics documentation (Italian)
```

## Implementation Details

### Dynamic Type Checking

The dynamic implementation (expModule.ml) performs type checking at runtime. It uses OCaml's `Hashtbl` module to implement sets efficiently and includes runtime validation to ensure operations are type-safe.

### Static Type Checking

The static implementation (withStaticTypeCheck.ml) adds a type checker that validates operations before execution, catching type errors earlier. It implements a type system to verify that set operations respect type constraints.

## Usage Examples

### Creating Sets

```ocaml
(* Empty set *)
let emptStr = eval (Empty(String)) emptyEnv;;
let emptInt = eval (Empty(Int)) emptyEnv;;

(* Singleton set *)
let singStr = eval (Singleton(CstString("Hello"), String)) emptyEnv;;
let singInt = eval (Singleton(CstInt(17), Int)) emptyEnv;;

(* Range set *)
let rangeSet = eval (Of(Int, Sum(CstInt(5), CstInt(10)), CstInt(3))) emptyEnv;;
```

### Set Operations

```ocaml
(* Union *)
let newSet = eval (Union(set1, set2)) emptyEnv;;

(* Insert element *)
let updatedSet = eval(Insert(CstInt(23), mySet)) emptyEnv;;

(* Check if element exists *)
let exists = eval (IsIn(CstString("Hello"), mySet)) emptyEnv;;

(* Functional operations *)
let pred = Fun("x", Greater(Den("x"), CstInt(10)));;
let filteredSet = eval (Filter(pred, mySet)) emptyEnv;;
```

## Running Tests

The project includes comprehensive test cases in testCase.ml and TestCaseForStaticChecker.ml that demonstrate all supported operations.

## Project Requirements

This project fulfills the following requirements:

1. Definition of operational rules for the set data type
2. Definition of operational rules for set operations
3. Implementation of an OCaml interpreter with static scoping
4. Implementation of dynamic type checking
5. Validation through comprehensive test cases
6. Implementation of static type checking (optional requirement)

For more detailed information, refer to the relazione.pdf and semantica.pdf documents.
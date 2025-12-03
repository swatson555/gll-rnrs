# gll-rnrs

**A monadic GLL parser combinator library for R6RS Scheme.**

## Description

This R6RS library implements **Generalized LL (GLL)** parsing using a monadic combinator interface. It is designed as a modern alternative to **sllgen** for students and researchers working through *Essentials of Programming Languages* (Friedman & Wand).

While `sllgen` is restricted to LL(1) grammars, `gll-rnrs` handles **left-recursion** and **ambiguity** natively. This allows you to implement grammars (LET, PROC, LETREC, EXPLICIT-REFS, IMPLICIT-REFS, MUTABLE-PAIRS, etc.) from the book directly or with enhancements without refactoring them to be right-recursive.

## Usage

### Defining a Grammar

The library provides combinators like `alt`, `seq`, and `red` (reduce) to define grammars concisely. This example demonstrates a standard expression grammar that handles precedence and left recursion.

```scheme
(import (gll))

(define-parser expr
  (alt (red (seq expr (string "+") term)
            (lambda (x _ y) (+ x y)))
       (red (seq expr (string "-") term)
            (lambda (x _ y) (- x y)))
       term))

(define-parser term
  (alt (red (seq term (string "*") factor)
            (lambda (x _ y) (* x y)))
       (red (seq term (string "/") factor)
            (lambda (x _ y) (/ x y)))
       factor))

(define-parser factor
  (alt (red (seq (string "(") expr (string ")"))
            (lambda (_ x __) x))
       num))

(define-parser digit
  (alt (string "0")
  (alt (string "1")
  (alt (string "2")
  (alt (string "3")
  (alt (string "4")
  (alt (string "5")
  (alt (string "6")
  (alt (string "7")
  (alt (string "8")
       (string "9")))))))))))

(define-parser num
  (red digit string->number))
```

### Running the Parser

The parser functions evaluate the monadic chain against a string.

```scheme
(expr "1*2+3*4")
;; => ((success 14 "") . #<procedure>)

(expr "9-(5+2)")
;; => ((success 2 "") . #<procedure>)
```

## References

* Daniel Spiewak. [GLL Combinators](https://epsil.github.io/gll/). The functional derivation of the algorithm used in this library.

* **Elizabeth Scott and Adrian Johnstone.** "GLL Parsing." Electronic Notes in Theoretical Computer Science (2010).

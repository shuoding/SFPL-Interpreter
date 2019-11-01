# SFPL

SFPL (Simple Functional Programming Language) is a very small dynamically typed functional programming language. It supports basic integer arithmetic and comparison operations, the ```let``` binding, the ```if``` expression, function definitions and function calls with lexical scoping, recursive functions, higher order functions, the ```pair``` data structure, and simple integer input and output.

This repository contains an interpreter of SFPL. The interpreter is written in Haskell, but for simplicity, it doesn't contain a ```main``` function. To run the interpreter, you can load it to GHCi and call the ```interpret``` function with your SFPL program (written as a Haskell String) directly.

## SFPL Tutorial

SFPL's syntax is similar to Lisp's syntax. A SFPL program is essentially an expression. An expression could be in one of the following forms.

#### Integer Literals: ```<nonnegative-integer>```

You can only write nonnegative integer literals. For negative integer literals, please use integer operators. For example, ```-7``` should be written as something like ```(- 0 7)```, or ```(call (function neg x (- 0 x)) 7)``` if you like.

#### The Nil Literal: ```nil```

This is a special value representing nothing.

#### Integer Operations: ```(<integer-operator> <expression-1> <expression-2>)```

SFPL supports 10 basic integer operators: ```+ - * / == != < <= > >=```. The meaning of them are clear. The values of comparison operations are also integers, where ```1``` indicates true and ```0``` indicates false.

```<expression-1>``` and ```<expression-2>``` must evaluate to integers, otherwise the interpreter will terminate and print an error message.

#### Pair Construction: ```(pair <expression-1> <expression-2>)```

```pair``` is used to construct pairs from two arbitrary valid expressions.

The types of ```<expression-1>``` and ```<expression-2>``` don't need to be the same. Actually, you can use ```pair``` and ```nil``` together to imitate lists. For example, ```(pair 1 (pair 2 (pair 3 nil)))``` could be used to represent a list containing ```1```, ```2```, and ```3```.

#### Extracting Components from Pairs: ```(first <expression>)``` and ```(second <expression>)```

```first``` and ```second``` act like what your intuition tells you.

The types of ```<expression>``` must be pairs, otherwise the interpreter will terminate and print an error message.

#### Let Binding: ```(let <variable> <expression-1> <expression-2>)```

This is an easy way to introduce variables. For example, ```(let x 1 (+ x 2))``` evaluates to ```3```. If there are nested ```let``` bindings introducing duplicate variable names, the inner one is effective.

#### If Expression: ```(if <condition-expression> <expression-1> <expression-2>)```

If ```<condition-expression>```'s value is zero, the ```if``` expression evaluates to ```<expression-1>```'s value. Otherwise, the ```if``` expression evaluates to ```<expression-2>```'s value.

Only the chosen branch will be evaluated, which implies some errors and nonterminating cases could be bypassed.

Since SFPL is a dynamically-typed language, the types of ```<expression-1>``` and ```<expression-1>``` don't need to be the same. However, ```<condition-expression>``` must evaluate to an integer, otherwise the interpreter will terminate and print an error message.

#### Function Definition: ```(function <function-name-variable> <parameter-variable> <function-body-expression>)```

SFPL only supports one parameter functions, but you can imitate multi-parameter functions via currying, since higher order functions are supported.

You can call ```<function-name-variable>``` in ```<function-body-expression>``` to define recursive functions.

This whole expression evaluates to a closure.

Since SFPL is a dynamically-typed language, there is no type associated to ```<parameter-variable>```, and one function can return different types of values based on its argument's type.

#### Function Call: ```(call <closure-expression> <argument-expression>)```

```<closure-expression>``` must evaluate to a closure, otherwise the interpreter will terminate and print an error message.

#### Type Testers: ```(<type-tester> <expression>)```

SFPL supports 4 type testers: ```isNil```, ```isInt```, ```isClosure```, and ```isPair```. The return values of those testers are integers, where ```1``` indicates true and ```0``` indicates false.

#### Input and Output: ```getIntLine``` and ```(putIntLine <expression>)```

SFPL only supports integer input and output. Each line is treated as an integer. ```getIntLine``` reads one line from the standard input and tries to convert it to an integer. If it succeeds, that ```getIntLine``` evaluates to that integer. If it fails, the interpreter will terminate and print an error. ```putIntLine``` prints an integer as one line to the standard output and evaluates to ```nil```. ```<expression>``` must evaluate to an integer, otherwise the interpreter will terminate and print an error.

## Examples

    *Main> interpret "(+ 1 2)"
    3
    *Main> interpret "(putIntLine (+ 1 getIntLine))"
    16
    17
    nil
    *Main> interpret "(call (function factorial n (if (== n 0) 1 (* n (call factorial (- n 1))))) 6)"
    720

## GHC Versions

This program has been tested on ```GHCi, version 8.6.5```.

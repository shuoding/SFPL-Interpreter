# SFPL

SFPL (Small Functional Programming Language) is a very small dynamically typed functional programming language. It supports basic integer arithmetic and comparison operations, the ```let``` binding, the ```if``` expression, function definitions and function calls with lexical scoping, recursive functions, higher order functions, the ```pair``` data structure, and simple integer input and output.

This repository contains a SFPL interpreter written in Haskell. For simplicity, it doesn't contain a ```main``` function, and you can load it into GHCi and call the ```interpret``` function with your SFPL program (a string) as the argument.

## SFPL Tutorial

SFPL's syntax is similar to Lisp's syntax. A SFPL program is essentially an expression, which could be in one of the following forms.

#### 1. Integer Literals: ```<nonnegative-integer>```

You can only write nonnegative integer literals. For negative integer literals, please use integer operators. For example, ```-7``` could be written as something like ```(- 0 7)```, or ```(call (function neg x (- 0 x)) 7)``` if you like.

#### 2. The Nil Literal: ```nil```

This is a special value representing nothing.

#### 3. Integer Operations: ```(<integer-operator> <expression-1> <expression-2>)```

SFPL supports 10 basic integer operators: ```+ - * / == != < <= > >=```. The meaning is clear. The values of comparison expressions are also integers, where ```1``` indicates true and ```0``` indicates false.

```<expression-1>``` evaluates to its value before ```<expression-2>```.

```<expression-1>``` and ```<expression-2>``` must evaluate to integers, otherwise the interpreter will terminate and print an error message.

#### 4. Pair Construction: ```(pair <expression-1> <expression-2>)```

```pair``` is used to construct pairs from two arbitrary valid expressions.

```<expression-1>``` evaluates to its value before ```<expression-2>```.

The types of ```<expression-1>``` and ```<expression-2>``` don't need to be the same. Actually, you can use ```pair``` and ```nil``` together to imitate lists, such as ```(pair 1 (pair 2 (pair 3 nil)))```.

#### 5. Extracting Components from Pairs: ```(first <expression>)``` and ```(second <expression>)```

```first``` and ```second``` act like what your intuition tells you.

The type of ```<expression>``` must be pair, otherwise the interpreter will terminate and print an error message.

#### 6. Let Binding: ```(let <variable> <expression-1> <expression-2>)```

This is an easy way to introduce variables. Variable names can only contain English letters. For example, ```(let x 1 (+ x 2))``` evaluates to ```3```. If there are nested ```let``` bindings introducing duplicate variable names, the innermost one is effective.

It acts like the call-by-value semantics, meaning that ```<expression-1>``` evaluates to its value before ```<expression-2>```.

#### 7. If Expression: ```(if <condition-expression> <expression-1> <expression-2>)```

If ```<condition-expression>```'s value is zero, the ```if``` expression evaluates to ```<expression-1>```'s value. Otherwise, the ```if``` expression evaluates to ```<expression-2>```'s value.

```<condition-expression>``` evaluates to its value first. Then, only the chosen branch will be evaluated, which implies some errors and nonterminating cases could be bypassed.

Since SFPL is a dynamically-typed language, the types of ```<expression-1>``` and ```<expression-1>``` don't need to be the same. However, ```<condition-expression>``` must evaluate to an integer, otherwise the interpreter will terminate and print an error message.

#### 8. Function Definition: ```(function <function-name-variable> <parameter-variable> <function-body-expression>)```

SFPL only supports one parameter functions, but you can imitate multi-parameter functions via currying, since higher order functions are supported.

You can call ```<function-name-variable>``` in ```<function-body-expression>``` to define recursive functions. Notice that actually you can only use ```<function-name-variable>``` in ```<function-body-expression>```, and in order to call the function elsewhere, you can either call the function definition directly (something like ```(call (function f x x) 1)```) or use the ```let``` binding to create a new name for the function for later use.

The whole function definition expression evaluates to a closure.

Since SFPL is a dynamically-typed language, there is no type associated to ```<parameter-variable>```, and one function can return different types of values based on its argument's type.

#### 9. Function Call: ```(call <closure-expression> <argument-expression>)```

```<closure-expression>``` evaluates to its value before ```<argument-expression>```.

```<closure-expression>``` must evaluate to a closure, otherwise the interpreter will terminate and print an error message.

#### 10. Type Testers: ```(<type-tester> <expression>)```

SFPL supports 4 type testers: ```isNil```, ```isInt```, ```isClosure```, and ```isPair```. The return values of those testers are integers, where ```1``` indicates true and ```0``` indicates false.

#### 11. Input and Output: ```getIntLine``` and ```(putIntLine <expression>)```

SFPL only supports integer input and output. Each line is treated as an integer. ```getIntLine``` reads one line from the standard input and tries to convert it to an integer. If it succeeds, that ```getIntLine``` evaluates to that integer. If it fails, the interpreter will terminate and print an error. ```putIntLine``` prints an integer as one line to the standard output and evaluates to ```nil```. ```<expression>``` must evaluate to an integer, otherwise the interpreter will terminate and print an error.

When you use ```getIneLine``` and ```putIntLine```, please pay attention to the evaluation order.

## Examples

    *Main> interpret "(+ 1 2)"
    3
    *Main> interpret "(putIntLine (+ 1 getIntLine))"
    16
    17
    nil
    *Main> interpret "(call (function factorial n (if (== n 0) 1 (* n (call factorial (- n 1))))) 6)"
    720

## A Complex Example

Here is a SFPL implementation of Quicksort. Specifically, it first reads a integer n from the standard input, and then reads n integers from the standard input, sorts them, and prints them to the standard output.

You may copy this program to ```quicksort.sfpl``` and do something like ```readFile "quicksort.sfpl" >>= interpret``` in GHCi. Then you can manually type your input and see the results. Notice that you can type some negative integers among those n integers that will be sorted. The restriction of nonnegative integers is only applicable to integer literals in the SFPL source code.

```
(let printList
(function pl l
	(if (isNil l)
		nil
		(let dummy (putIntLine (first l)) (call pl (second l)))
	)
)
(let getList
(function gl n
	(if (== 0 n)
		nil
		(pair getIntLine (call gl (- n 1)))
	)
)
(let append
(function apA l
	(function apB x
		(if (isNil l)
			(pair x nil)
			(pair (first l) (call (call apA (second l)) x))
		)
	)
)
(let filterLess
(function flA x
	(function flB l
		(if (isNil l)
			nil
			(if (< (first l) x)
				(pair (first l) (call (call flA x) (second l)))
				(call (call flA x) (second l))
			)
		)
	)
)
(let filterGreaterEqual
(function fgeA x
	(function fgeB l
		(if (isNil l)
			nil
			(if (>= (first l) x)
				(pair (first l) (call (call fgeA x) (second l)))
				(call (call fgeA x) (second l))
			)
		)
	)
)
(let concatenate
(function catA la
	(function catB lb
		(if (isNil la)
			lb
			(pair (first la) (call (call catA (second la)) lb))
		)
	)
)
(let quickSort
(function qs l
	(if (isNil l)
		nil
		(let pivot (first l)
		(let lHalf (call (call filterLess pivot) (second l))
		(let rHalf (call (call filterGreaterEqual pivot) (second l))
		(let lHalfSorted (call qs lHalf)
		(let rHalfSorted (call qs rHalf)
		(let lHalfSortedAndPivot (call (call append lHalfSorted) pivot)
			(call (call concatenate lHalfSortedAndPivot) rHalfSorted)
		))))))
	)
)
(call printList (call quickSort (call getList getIntLine)))
)))))))
```

## GHC Version

This program has been tested on ```GHCi, version 8.6.5```.

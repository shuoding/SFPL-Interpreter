# A Toy Interpreter for a Toy Language

## Syntax
```
prog -> num | var
      | "(" "lambda" var prog ")"
      | "(" "call" prog prog ")"
      | "(" "let" var "=" prog "in" prog ")"
      | "(" "if" prog "then" prog "else" prog ")"
      | "(" "num?" prog ")"
      | "(" "closure?" prog ")"
      | "(" "+" prog prog ")" | "(" "-" prog prog ")" | "(" "*" prog prog ")" | "(" "/" prog prog ")"
      | "(" "==" prog prog ")" | "(" "!=" prog prog ")" | "(" "<" prog prog ")" | "(" "<=" prog prog ")" | "(" ">" prog prog ")" | "(" ">=" prog prog ")"
num -> <exact numbers in Racket>
var -> <symbols in Racket>
```

## Semantics
Please see the code.

## Examples
```Racket> (interpret 10)
10
> (interpret '(lambda x x))
(closure '(lambda x x) '())
> (interpret '(call (lambda x (+ x x)) 10))
20
> (interpret '(let x = 10 in (+ x x)))
20
> (interpret '(if (== 0 0) then 1 else 0))
1
> (interpret '(num? 10))
1
> (interpret '(closure? (lambda x x)))
1
> (interpret '(let factorial =
              (lambda n
                (if (== n 0)
                    then 1
                    else (* n (call self (- n 1)))))
              in (call factorial 10)))
3628800
> (interpret '(let x = 10 in
              (let f = (lambda y (+ x y)) in
                (let x = 0 in
                  (call f 10)))))
20
```

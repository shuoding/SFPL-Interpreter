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

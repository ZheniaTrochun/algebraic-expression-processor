# Simple processor of algebraic expressions

### Algebra definition

```
    E --> E "+" E
        | E "-" E
        | "-" E
        | E "*" E
        | E "/" E
        | E "^" E
        | "(" E ")"
        | v
```
  
Where:  
- Parentheses have precedence over all operators.
- ^ (exponentiation) has precedence over unary - and the binary operators /, *, -, and +.
- "*" and / have precedence over unary - and binary - and +.
- Unary - has precedence over binary - and +.
- ^ is right associative while all other binary operators are left associative.
  
This can be simplified:
```
    E --> P {B P}
    P --> v | "(" E ")" | U P
    B --> "+" | "-" | "*" | "/" | "^"
    U --> "-"
```
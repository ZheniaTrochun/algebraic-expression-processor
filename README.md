# Simple processor of algebraic expressions

### Algebra definition

```
    E --> E "+" E
        | E "-" E
        | "-" E
        | E "*" E
        | E "/" E
        | "(" E ")"
        | v
```
  
Where:  
- Parentheses have precedence over all operators.
- ^ (exponentiation) has precedence over unary - and the binary operators /, *, -, and +.
- "*" and / have precedence over unary - and binary - and +.
- Unary - has precedence over binary - and +.
  
This can be simplified to:
```
    E --> P {B P}
    P --> v | "(" E ")" | U P
    B --> "+" | "-" | "*" | "/"
    U --> "-"
```
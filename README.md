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

Opening brackets examples:
```
(a*(c+d))*(x-y*(z+k))
->  a*(x-y*(z+k))*(c+d)
->  (a*x-y*a*(z+k))*(c+d)
->  (a*x-y*a*z-y*a*k)*(c+d)
->  a*x*(c+d)-y*a*z*(c+d)-y*a*k*(c+d)
->  a*x*c+a*x*d-y*a*z*c+y*a*z*d-y*a*k*c+y*a*k*d  
```

```
(a+b)*(c+d)
->  a*(c+d)+b*(c+d)
->  a*c+a*d+b*c+b*d  
```

```
(a*(c+d))*(x+y)
->  a*(x+y)*(c+d)
->  (a*x+a*y)*(c+d)
->  a*x*c + a*y*c + a*x*d + a*y*d  
```

```
(a+b+2)/(a-3*x)*123
-> (a*123+b*123+2*123)/(a-3*x)
-> (a*123)/(a-3*x)+(b*123)/(a-3*x)+(2*123)/(a-3*x)
```
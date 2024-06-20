<div align="center">
    <h1><code>N A M U L</code></h1>
    <p><i>think less, code faster.</i></p>
</div>

Namul is a programming language for competitive programming.

## Hello World

A *format string* is an expression that prints formatted text. 
```namul
`Hello, World!`;
```

## A + B

Use input statement to define and read into variables.
```namul
|i32 A, i32 B|
i32 answer = A + B;
`$answer`;
```

## A + B with multiple tests

Repeat blocks `T` times.
```namul
|i32 T|
rep T {
    |i32 A, i32 B|;
    i32 C = A + B;
    `$C\n`;
}
```

## Lunar year calculation

Comparisons can be chained. Format strings are expressions to allow use in conditional operations.
```namul
|i32 year|
year % 4 == 0 != year % 100 || year % 400 == 0 ? `1` : `0`;
```

## Greatest Common Divisor

Declarations and assignments matches patterns.
```namul
|i32 a, i32 b|
(i32 c, i32 d) = (a, b);
while d != 0 {
    (c, d) = (d, c % d);
}
d = a / c * b;
`$c\n$d`;
```

Or you can use classic recursive function:
```namul
|i32 a, i32 b|
fn gcd(i32 a, i32 b) i32 {
    b == 0 ? a : gcd(b, a % b)
}
i32 c = gcd(a, b);
i32 d = a / c * b;
`$c\n$d`;
```

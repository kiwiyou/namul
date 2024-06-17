<div align="center">
    <h1><code>N A M U L</code></h1>
    <p><i>think less, code faster.</i></p>
</div>

Namul is a programming language for competitive programming.

## Hello World

A *format string* is a statement that prints formatted text. 
```namul
`Hello, World!`;
```

## A + B

Use input statement to define and read into variables.
```namul
|i32 A, i32 B|;
i32 answer = A + B;
`$answer`;
```

## A + B with multiple tests

Repeat blocks `T` times.
```namul
|i32 T|;
rep T {
    |i32 A, i32 B|;
    i32 C = A + B;
    `$C\n`;
}
```
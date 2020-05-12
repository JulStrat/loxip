# loxip

loxip - Free Pascal interpreter implementation of Lox programming language by [Bob Nystrom](https://github.com/munificent).

Work in progress.

## Challenges

### Chapter 7.Evaluating Expressions

2. Define `+` such that if either operand is a string, 
the other is converted to a string and the results are then concatenated. 

3. Division by zero.
   
## AST visualization

Statement `printdot expression;` prints espression AST in [DOT format](https://en.wikipedia.org/wiki/DOT_(graph_description_language)).

```
> printdot 1 + 8 * 89 - - 12;
[DEBUG] (TBinaryExpression) Accepting binary expression - -
[DEBUG] (TBinaryExpression) Accepting binary expression - +
[DEBUG] (TLiteralExpression) Accepting literal expression - 1
[DEBUG] (TBinaryExpression) Accepting binary expression - *
[DEBUG] (TLiteralExpression) Accepting literal expression - 8
[DEBUG] (TLiteralExpression) Accepting literal expression - 89
[DEBUG] (TUnaryExpression) Accepting unary expression - -
[DEBUG] (TLiteralExpression) Accepting literal expression - 12
digraph astgraph {
node [shape=circle, fontsize=10, fontname="Courier"];
rankdir = BT;
0 [label="-"]
1 [label="+"]
2 [label="1", shape=rectangle]
2 -> 1
3 [label="*"]
4 [label="8", shape=rectangle]
4 -> 3
5 [label="89", shape=rectangle]
5 -> 3
3 -> 1
1 -> 0
6 [label="-"]
7 [label="12", shape=rectangle]
7 -> 6
6 -> 0
}

>
```

AST for expression `1 + 8 * 89 - - 12`

<img src="https://github.com/JulStrat/loxip/blob/devop/graph.png">

## Interpreter

<img src="https://github.com/JulStrat/loxip/blob/devop/loxip.JPG">
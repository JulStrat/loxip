# loxip

[Lox interpreter](http://www.craftinginterpreters.com/) written in Free Pascal.

Work in progress.

## Challenges

### Chapter 5.Representing Code 

3. Reverse Polish Notation.
```
> print (1 + 2) * (4 - 3);
[DEBUG] (TASTPrinter) (* (group (+ 1 2)) (group (- 4 3)))
[DEBUG] (TASTPrinter RPN) 1 2 + 4 3 - *
```

### Chapter 7.Evaluating Expressions

1. String comparison.
```
> print "abd" > "aba";
[DEBUG] (TBinaryExpression.Accept) Accepting binary expression: >
[DEBUG] (TLiteralExpression.Accept) Accepting literal expression: abd
[DEBUG] (TLiteralExpression.Accept) Accepting literal expression: aba
True
Tick count - 0.
> print "abd" < "aba";
[DEBUG] (TBinaryExpression.Accept) Accepting binary expression: <
[DEBUG] (TLiteralExpression.Accept) Accepting literal expression: abd
[DEBUG] (TLiteralExpression.Accept) Accepting literal expression: aba
False
Tick count - 0.
```
2. Define `+` such that if either operand is a string, 
the other is converted to a string and the results are then concatenated. 
```
> print "Hello " + 2020 + " " + true;
[DEBUG] (TASTPrinter) (+ (+ (+ "Hello " 2020) " ") True)
[DEBUG] (TASTPrinter RPN) "Hello " 2020 + " " + True +
[DEBUG] (TInterpreter) Evaluating TBinaryExpression.
[DEBUG] (TInterpreter) Evaluating TBinaryExpression.
[DEBUG] (TInterpreter) Evaluating TBinaryExpression.
[DEBUG] (TInterpreter) Evaluating TLiteralExpression.
[DEBUG] (TInterpreter) Evaluating TLiteralExpression.
[DEBUG] (TInterpreter) Evaluating TLiteralExpression.
[DEBUG] (TInterpreter) Evaluating TLiteralExpression.
Hello 2020 True
```
3. Division by zero.
```
> 8/0;
[DEBUG] (TInterpreter) Evaluating TBinaryExpression.
[DEBUG] (TInterpreter) Evaluating TLiteralExpression.
[DEBUG] (TInterpreter) Evaluating TLiteralExpression.
[ERROR] Division by zero. [line 1].
```   
## AST visualization

`printdot expression;` generates AST in [DOT format](https://en.wikipedia.org/wiki/DOT_(graph_description_language)).
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

## Built With

* [Lazarus](https://www.lazarus-ide.org/) - The professional Free Pascal RAD IDE.

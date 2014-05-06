第四章，条件语句
=====

和其它的编程语句一样，Scheme 也包含条件语句。


最基本的结构就是if：

```scheme
(if 测试条件
    then-分支
    else-分支)
```

如果测试条件运算的结果是真(即，非`#f`的任何其它值)，`then`分支将会被运行(即满足条件时的运行分支)。否则，`else`分支会被运行。`else`分支是可选的。

```scheme
(define p 80)
 
(if (> p 70) 
    'safe
    'unsafe)
=>  safe 
 
(if (< p 90)
    'low-pressure) ;no ``else'' branch
=>  low-pressure 
```

为了方便，Scheme还提供了一些其它的条件结构语句。它们可以被定义成宏来扩充if表达式。


## 4.1 when 和 unless


当我们只需要一个基本条件语句分支时（”then”分支或”else”分支），使用when 和 unless会更方便。(这里的示例已经更换，原示例)

```scheme
(define a 10)
(define b 20)
(when (< a b)
       (display “a是”)
       (display a)
       (display “b是”)
       (display b)
       (display “a大于b” ) )
```

先判断a是否小于b，这个条件成立时会输出5条信息。


使用if实现相同的程序会是这样：

```scheme
(define a 10)
(define b 20)
(if (< a b)
       (begin
              (display “a是”)
              (display a)
              (display “b是”)
              (display b)
              (display “a大于b” ) ))
```

注意`when`的分支是一个隐式的`begin`语句结构，
而如果`if`的分支有多个代码结构时，需要一个显式的`begin`代码结构。


同样的功能还可以像下面这样用`unless`来写(`unless`和`when`的意思正好相反)：

```scheme
(define a 10)
(define b 20)
(unless (>= a b)
       (display “a是”)
       (display a)
       (display “b是”)
       (display b)
       (display “a大于b” ) )
```

并不是所有的Scheme环境都提供`when`和`unless`。
如果你的Scheme中没有，你可以用宏来自定义出`when`和`unless`(宏，见第8章)。



## 4.2 cond

`cond`结构在表示多重`if`表达式时很方便，
多重`if`结构除了最后一个`else`分支以外的其余分支都会包含一个新的`if`条件。因此，

```scheme
(if (char<? c #\c) -1
    (if (char=? c #\c) 0
        1))
```

这样的结构都可以使用`cond`来这样写：

```scheme
(cond ((char<? c #\c) -1)
      ((char=? c #\c) 0)
      (else 1))
```

`cond`就是这样的一种多分支条件结构。每个从句都包含一个判断条件和一个相关的操作。第一个判断成立的从句将会引发它相关的操作执行。如果任何一个分支的条件判断都不成立则最后一个`else`分支将会执行(`else`分支语句是可选的)。


cond的分支操作都是`begin`结构。



## 4.3 case

当`cond`结构的每个测试条件是一个测试条件的分支条件时，可以缩减为一个`case`表达式。

```scheme
(define c #\c)
(case c
  ((#\a) 1)
  ((#\b) 2)
  ((#\c) 3)
  (else 4))
=>  3
```

分支头值是` #\c` 的分支将被执行。



## 4.4 and 和 or


Scheme提供了对boolean值进行逻辑与`and`和逻辑或`or`运算的结构。(我们已经见过了布尔类型的求反运算not过程。)


当所有子结构的值都是真时，`and`的返回值是真，实际上，`and`的运行结果是最后一个子结构的值。如果任何一个子结构的值都是假，则返回`#f`。

```scheme
(and 1 2)  =>  2
(and #f 1) =>  #f
```

而`or`会返回它第一个为值为真的子结构的结果。如果所有的子结构的值都为假，`or`则返回`#f`。

```scheme
(or 1 2)  =>  1
(or #f 1) =>  1
```

`and`和`or`都是从左向右运算。当某个子结构可以决定最终结果时，`and`和`or`会忽略剩余的子结构，即它们是“短路”的。

```scheme
(and 1 #f expression-guaranteed-to-cause-error)
=>  #f
 
(or 1 #f expression-guaranteed-to-cause-error)
=>  1
```
